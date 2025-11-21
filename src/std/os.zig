const std = @import("std");
const stdx = @import("stdx");
const fatal = cy.fatal;
const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const vmc = cy.vmc;
const rt = cy.rt;
const Value = cy.Value;
const fmt = @import("../fmt.zig");
const bindings = @import("../builtins/bindings.zig");
const core = @import("../builtins/core.zig");
const zErrFunc = core.zErrFunc;
const cFunc = core.cFunc;
const Symbol = bindings.Symbol;
const bt = cy.types.BuiltinTypes;
const ffi = @import("os_ffi.zig");
const http = @import("../http.zig");
const cache = @import("../cache.zig");
const cli = @import("../cli.zig");

const log = cy.log.scoped(.os);

pub const Src = @embedFile("os.cy");

/// Adjusted by the CLI main when args are used to interpret a script file.
pub var argv_start: usize = 0;

const funcs = [_]struct{[]const u8, C.BindFunc}{
    // Top level
    .{"cacheUrl",       zErrFunc(cacheUrl)},
    .{"fetchUrl",       zErrFunc(fetchUrl)},
    
    .{"stderr",         zErrFunc(stderr)},
    .{"stdin",          zErrFunc(stdin)},
    .{"stdout",         zErrFunc(stdout)},
    .{"exec",           zErrFunc(exec)},
    .{"_args",          zErrFunc(_args)},
    .{"openLib",        zErrFunc(openLib)},
};

const types = [_]struct{[]const u8, C.BindType}{
    // .{"FFI",          CS.TYPE_HOBJ(null, ffi.FFI_deinit)},
};

pub fn bind(_: *cy.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }

    for (types) |e| {
        C.mod_add_type(mod, e.@"0", e.@"1");
    }

    C.mod_add_global(mod, "vecBitSize", @ptrCast(&load_vec_bit_size));
}

fn load_vec_bit_size(_: *cy.VM, out: *Value) callconv(.c) void {
    if (std.simd.suggestVectorLength(u8)) |VecSize| {
        out.* = cy.Value.initInt(VecSize * 8);
    } else {
        out.* = cy.Value.initInt(0);
    }
}

const File = extern struct {
    fd: std.c.fd_t,
    closed: bool = false,
};

fn stderr(t: *cy.Thread) !C.Ret {
    if (!cy.hasStdFiles) return t.ret_panic("Unsupported.");

    const ret = t.ret(File);
    const handle = std.fs.File.stderr().handle;
    ret.* = .{ .fd = handle };
    return C.RetOk;
}

fn stdin(t: *cy.Thread) !C.Ret {
    if (!cy.hasStdFiles) return t.ret_panic("Unsupported.");

    const ret = t.ret(File);
    const handle = std.fs.File.stdin().handle;
    ret.* = .{ .fd = handle };
    return C.RetOk;
}

fn stdout(t: *cy.Thread) !C.Ret {
    if (!cy.hasStdFiles) return t.ret_panic("Unsupported.");

    const ret = t.ret(File);
    const handle = std.fs.File.stdout().handle;
    ret.* = .{ .fd = handle };
    return C.RetOk;
}

const VarEntry = struct{ ?cy.Value, ?*const fn(*cy.VM, *cy.Type) anyerror!cy.Value };

const vars = std.StaticStringMap(VarEntry).initComptime(.{
});

fn global_loader(vm_: ?*C.VM, v: C.VarInfo, out: [*c]C.Value) callconv(.c) bool {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const name = C.from_bytes(v.name);
    if (vars.get(name)) |entry| {
        if (entry.@"0") |value| {
            out.* = @bitCast(value);
        } else {
            out.* = @bitCast(entry.@"1".?(vm, @ptrCast(@alignCast(v.type))) catch @panic("error"));
        }
        return true;
    }
    return false;
}

fn _args(t: *cy.Thread) !C.Ret {
    if (cy.isWasm) return t.ret_panic("Unsupported.");

    const ret = t.ret(cy.heap.Slice);
    const byte_buffer_t: cy.TypeId = @intCast(t.param(i64));

    var args: std.ArrayListUnmanaged(cy.heap.Str) = .{};
    defer {
        for (args.items) |*arg| {
            t.heap.destructStr(arg);
        }
        args.deinit(t.alloc);
    }

    var iter = try std.process.argsWithAllocator(t.alloc);
    defer iter.deinit();
    while (iter.next()) |arg| {
        const str = try t.heap.init_str(arg);
        try args.append(t.alloc, str);
    }

    const final_args = args.items[argv_start..];

    var slice = try t.heap.init_slice_undef(byte_buffer_t, final_args.len, @sizeOf(cy.heap.Str));
    @memcpy(slice.items(cy.heap.Str), final_args);
    try args.resize(t.alloc, argv_start);
    ret.* = slice;
    return C.RetOk;
}

extern fn hostSleep(secs: u64, nsecs: u64) void;

pub fn openLib(t: *cy.Thread) anyerror!C.Ret {
    if (!cy.hasFFI) return t.ret_panic("Unsupported.");

    const ret = t.ret(ffi.DynLib);

    const config: ffi.BindLibConfig = .{};
    return @call(.never_inline, ffi.ffiBindLib, .{t, config, ret});
}

pub extern fn hostFileWrite(fid: u32, str: [*]const u8, strLen: usize) void;

fn cacheUrl(t: *cy.Thread) anyerror!C.Ret {
    if (cy.isWasm) return t.ret_panic("Unsupported.");
    const ret = t.ret(cy.heap.Str);
    const url = t.param(cy.heap.Str).slice();
    const path = try allocCacheUrl(t.c.vm, url);
    defer t.alloc.free(path);
    ret.* = try t.heap.init_str(path);
    return C.RetOk;
}

pub fn allocCacheUrl(vm: *cy.VM, url: []const u8) ![]const u8 {
    const specGroup = try cache.getSpecHashGroup(vm.alloc, url);
    defer specGroup.deinit(vm.alloc);

    if (vm.config.reload) {
        try specGroup.markEntryBySpecForRemoval(url);
    } else {
        // First check local cache.
        if (try specGroup.findEntryBySpec(url)) |entry| {
            return cache.allocSpecFilePath(vm.alloc, entry);
        }
    }

    const resp = try http.get(vm.alloc, vm.httpClient, url);
    defer vm.alloc.free(resp.body);
    if (resp.status != .ok) {
        log.tracev("cacheUrl response status: {}", .{resp.status});
        return error.UnknownError;
    } else {
        const entry = try cache.saveNewSpecFile(vm.alloc, specGroup, url, resp.body);
        defer entry.deinit(vm.alloc);
        return cache.allocSpecFilePath(vm.alloc, entry);
    }
}

const ExecResult = extern struct {
    out: cy.heap.Str,
    err: cy.heap.Str,
    code: i64,
};

pub fn exec(t: *cy.Thread) anyerror!C.Ret {
    if (cy.isWasm) return t.ret_panic("Unsupported.");

    const ret = t.ret(ExecResult);
    const args = t.param(cy.heap.Slice).items(cy.heap.Str);
    var buf: std.ArrayListUnmanaged([]const u8) = .{};
    defer {
        buf.deinit(t.alloc);
    }
    for (args) |arg| {
        try buf.append(t.alloc, arg.slice());
    }

    const res = try std.process.Child.run(.{
        .allocator = t.alloc,
        .argv = buf.items,
        .max_output_bytes = 1024 * 1024 * 10,
    });

    defer t.alloc.free(res.stdout);
    var out = try t.heap.init_str(res.stdout);
    errdefer t.heap.destructStr(&out);
    defer t.alloc.free(res.stderr);
    var err = try t.heap.init_str(res.stderr);
    errdefer t.heap.destructStr(&err);
    var code: i64 = -1;
    if (res.term == .Exited) {
        code = @intCast(res.term.Exited);
    }

    ret.* = .{
        .out = out,
        .err = err,
        .code = code,
    };
    return C.RetOk;
}

pub fn fetchUrl(t: *cy.Thread) anyerror!C.Ret {
    if (cy.isWasm) return t.ret_panic("Unsupported.");

    const ret = t.ret(cy.heap.Str);
    const url = t.param(cy.heap.Str).slice();
    if (cy.isWasm) {
        hostFetchUrl(url.ptr, url.len);
        ret.* = try t.heap.init_str("");
    } else {
        const resp = try http.get(t.alloc, t.c.vm.httpClient, url);
        defer t.alloc.free(resp.body);
        // TODO: Use allocOwnedString
        ret.* = try t.heap.init_str(resp.body);
    }
    return C.RetOk;
}

extern fn hostFetchUrl(url: [*]const u8, urlLen: usize) void;
