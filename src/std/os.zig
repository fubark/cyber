const std = @import("std");
const stdx = @import("stdx");
const builtin = @import("builtin");
const C = @import("../capi.zig");
const fmt = @import("../fmt.zig");
const bindings = @import("../builtins/bindings.zig");
const cli = @import("../cli.zig");
const zErrFunc = cli.zErrFunc;
const Symbol = bindings.Symbol;
// const ffi = @import("os_ffi.zig");
const http = @import("../http.zig");
const cache = @import("../cache.zig");
const is_wasm = builtin.cpu.arch.isWasm();

const log = std.log.scoped(.os);

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

pub fn bind(_: *C.VM, mod: *C.Sym) callconv(.c) void {
    for (funcs) |e| {
        C.mod_add_func(mod, e.@"0", e.@"1");
    }

    for (types) |e| {
        C.mod_add_type(mod, e.@"0", e.@"1");
    }

    C.mod_add_global(mod, "vecBitSize", C.BIND_GLOBAL(&simd_bit_size));
}

var simd_bit_size: i64 = if (std.simd.suggestVectorLength(u8)) |VecSize|
    VecSize * 8 else 0;

const File = extern struct {
    fd: std.c.fd_t,
    closed: bool = false,
};

fn stderr(t: *C.Thread) !C.Ret {
    // if (!cy.hasStdFiles) return t.ret_panic("Unsupported.");

    const ret = C.thread_ret(t, File);
    const handle = std.fs.File.stderr().handle;
    ret.* = .{ .fd = handle };
    return C.RetOk;
}

fn stdin(t: *C.Thread) !C.Ret {
    // if (!cy.hasStdFiles) return t.ret_panic("Unsupported.");

    const ret = C.thread_ret(t, File);
    const handle = std.fs.File.stdin().handle;
    ret.* = .{ .fd = handle };
    return C.RetOk;
}

fn stdout(t: *C.Thread) !C.Ret {
    // if (!cy.hasStdFiles) return t.ret_panic("Unsupported.");

    const ret = C.thread_ret(t, File);
    const handle = std.fs.File.stdout().handle;
    ret.* = .{ .fd = handle };
    return C.RetOk;
}

fn _args(t: *C.Thread) !C.Ret {
    if (is_wasm) return t.ret_panic("Unsupported.");

    const ret = C.thread_ret(t, C.Slice);
    const byte_buffer_t: C.TypeId = @intCast(C.thread_int(t));

    const alloc = C.thread_allocator(t);

    var args: std.ArrayListUnmanaged(C.str) = .{};
    defer {
        for (args.items) |*arg| {
            C.str_deinit(t, arg);
        }
        args.deinit(alloc);
    }

    var iter = try std.process.argsWithAllocator(alloc);
    defer iter.deinit();
    while (iter.next()) |arg| {
        const str = C.str_init(t, arg);
        try args.append(alloc, str);
    }

    const final_args = args.items[argv_start..];

    const slice = C.slice_init(t, byte_buffer_t, final_args.len, @sizeOf(C.str));
    @memcpy(C.slice_items(slice, C.str), final_args);
    try args.resize(alloc, argv_start);
    ret.* = slice;
    return C.RetOk;
}

extern fn hostSleep(secs: u64, nsecs: u64) void;

pub fn openLib(t: *C.Thread) anyerror!C.Ret {
    // if (!cy.hasFFI) return t.ret_panic("Unsupported.");

    // const ret = C.thread_ret(t, ffi.DynLib);

    // const config: ffi.BindLibConfig = .{};
    // return @call(.never_inline, ffi.ffiBindLib, .{t, config, ret});
    return C.thread_ret_panic(t, "TODO");
}

pub extern fn hostFileWrite(fid: u32, str: [*]const u8, strLen: usize) void;

fn cacheUrl(t: *C.Thread) anyerror!C.Ret {
    if (is_wasm) return t.ret_panic("Unsupported.");

    const vm = C.thread_vm(t);
    const alloc = C.thread_allocator(t);

    const ret = C.thread_ret(t, C.str);
    const url = C.str_bytes(C.thread_str(t));
    const path = try allocCacheUrl(vm, url);
    defer alloc.free(path);
    ret.* = C.str_init(t, path);
    return C.RetOk;
}

pub fn allocCacheUrl(vm: *C.VM, url: []const u8) ![]const u8 {
    const alloc = C.vm_allocator(vm);

    const specGroup = try cache.getSpecHashGroup(alloc, url);
    defer specGroup.deinit(alloc);

    const a: *cli.App = @ptrCast(@alignCast(C.vm_user_data(vm)));
    if (a.config.reload) {
        try specGroup.markEntryBySpecForRemoval(url);
    } else {
        // First check local cache.
        if (try specGroup.findEntryBySpec(url)) |entry| {
            return cache.allocSpecFilePath(alloc, entry);
        }
    }

    const resp = try http.get(alloc, a.httpClient, url);
    defer alloc.free(resp.body);
    if (resp.status != .ok) {
        cli.tracev("cacheUrl response status: {}", .{resp.status});
        return error.UnknownError;
    } else {
        const entry = try cache.saveNewSpecFile(alloc, specGroup, url, resp.body);
        defer entry.deinit(alloc);
        return cache.allocSpecFilePath(alloc, entry);
    }
}

const ExecResult = extern struct {
    out: C.str,
    err: C.str,
    code: i64,
};

pub fn exec(t: *C.Thread) anyerror!C.Ret {
    if (is_wasm) return t.ret_panic("Unsupported.");

    const alloc = C.thread_allocator(t);

    const ret = C.thread_ret(t, ExecResult);
    const args_slice = C.thread_slice(t);
    const args = C.slice_items(args_slice, C.str);
    var buf: std.ArrayListUnmanaged([]const u8) = .{};
    defer {
        buf.deinit(alloc);
    }
    for (args) |arg| {
        try buf.append(alloc, C.str_bytes(arg));
    }

    const res = try std.process.Child.run(.{
        .allocator = alloc,
        .argv = buf.items,
        .max_output_bytes = 1024 * 1024 * 10,
    });

    defer alloc.free(res.stdout);
    var out = C.str_init(t, res.stdout);
    errdefer t.heap.destructStr(&out);
    defer alloc.free(res.stderr);
    var err = C.str_init(t, res.stderr);
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

pub fn fetchUrl(t: *C.Thread) anyerror!C.Ret {
    if (is_wasm) return t.ret_panic("Unsupported.");

    const ret = C.thread_ret(t, C.str);
    const url = C.str_bytes(C.thread_param(t, C.str));

    const vm = C.thread_vm(t);
    const alloc = C.thread_allocator(t);

    const a: *cli.App = @ptrCast(@alignCast(C.vm_user_data(vm)));
    const resp = try http.get(alloc, a.httpClient, url);
    defer alloc.free(resp.body);
    ret.* = C.str_init(t, resp.body);

    return C.RetOk;
}

extern fn hostFetchUrl(url: [*]const u8, urlLen: usize) void;

pub fn dlopen(path: []const u8) !std.DynLib {
    if (builtin.os.tag == .linux and builtin.link_libc) {
        const path_c = try std.posix.toPosixPath(path);
        // Place the lookup scope of the symbols in this library ahead of the global scope.
        const RTLD_DEEPBIND = 0x00008;
        return std.DynLib{
            .inner = .{
                .handle = std.c.dlopen(&path_c, std.c.RTLD.LAZY | RTLD_DEEPBIND) orelse {
                    return error.FileNotFound;
                },
            },
        };
    } else {
        return std.DynLib.open(path);
    }
}