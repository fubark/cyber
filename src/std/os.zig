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
const builtins = @import("../builtins/builtins.zig");
const prepThrowZError = builtins.prepThrowZError;
const zErrFunc = builtins.zErrFunc;
const Symbol = bindings.Symbol;
const prepareThrowSymbol = bindings.prepareThrowSymbol;
const bt = cy.types.BuiltinTypes;
const ffi = @import("os_ffi.zig");
const http = @import("../http.zig");
const cache = @import("../cache.zig");
const fs = @import("fs.zig");

const log = cy.log.scoped(.os);

pub var CArrayT: cy.TypeId = undefined;
pub var CDimArrayT: cy.TypeId = undefined;
pub var nextUniqId: u32 = undefined;

pub const Src = @embedFile("os.cy");

const func = cy.hostFuncEntry;
pub const funcs = [_]C.HostFuncEntry{
    // Top level
    func("access",         zErrFunc(access)),
    func("args",           zErrFunc(osArgs)),
    func("cacheUrl",       zErrFunc(cacheUrl)),
    func("copyFile",       zErrFunc(copyFile)),
    func("createDir",      zErrFunc(createDir)),
    func("createFile",     zErrFunc(createFile)),
    func("cstr",           zErrFunc(cstr)),
    func("cwd",            zErrFunc(cwd)),
    func("dirName",        zErrFunc(dirName)),
    func("execCmd",        zErrFunc(execCmd)),
    func("exePath",        zErrFunc(exePath)),
    func("exit",           exit),
    func("fetchUrl",       zErrFunc(fetchUrl)),
    func("free",           free),
    func("getEnv",         zErrFunc(getEnv)),
    func("getEnvAll",      zErrFunc(getEnvAll)),
    func("malloc",         zErrFunc(malloc)),
    func("milliTime",      milliTime),
    func("newFFI",         newFFI),
    func("now",            zErrFunc(now)),
    func("openDir",        zErrFunc(openDir)),
    func("openDir",        zErrFunc(openDir2)),
    func("openFile",       zErrFunc(openFile)),
    func("parseArgs",      zErrFunc(parseArgs)),
    func("readAll",        zErrFunc(readAll)),
    func("readFile",       zErrFunc(readFile)),
    func("readLine",       zErrFunc(readLine)),
    func("realPath",       zErrFunc(realPath)),
    func("removeDir",      zErrFunc(removeDir)),
    func("removeFile",     zErrFunc(removeFile)),
    func("setEnv",         zErrFunc(setEnv)),
    func("sleep",          sleep),
    func("unsetEnv",       unsetEnv),
    func("writeFile",      zErrFunc(writeFile)),

    // File
    func("File.close",          fs.fileClose),
    func("File.iterator",       zErrFunc(fs.fileIterator)),
    func("File.next",           zErrFunc(fs.fileNext)),
    func("File.read",           zErrFunc(fs.fileRead)),
    func("File.readAll",        zErrFunc(fs.fileReadAll)),
    func("File.seek",           zErrFunc(fs.fileSeek)),
    func("File.seekFromCur",    zErrFunc(fs.fileSeekFromCur)),
    func("File.seekFromEnd",    zErrFunc(fs.fileSeekFromEnd)),
    func("File.stat",           zErrFunc(fs.fileOrDirStat)),
    func("File.streamLines",    zErrFunc(fs.fileStreamLines)),
    func("File.streamLines",    zErrFunc(fs.fileStreamLines1)),
    func("File.write",          zErrFunc(fs.fileWrite)),

    // Dir
    func("Dir.iterator",   fs.dirIterator),
    func("Dir.stat",       zErrFunc(fs.fileOrDirStat)),
    func("Dir.walk",       fs.dirWalk),

    // DirIterator
    func("DirIterator.next", zErrFunc(fs.dirIteratorNext)),

    // FFI
    func("FFI.bindCallback",   zErrFunc(ffi.ffiBindCallback)),
    func("FFI.bindLib",        zErrFunc(bindLib)),
    func("FFI.bindLib2",       zErrFunc(bindLibExt)),
    func("FFI.bindObjPtr",     zErrFunc(ffi.ffiBindObjPtr)),
    func("FFI.cbind",          zErrFunc(ffi.ffiCbind)),
    func("FFI.cfunc",          zErrFunc(ffi.ffiCfunc)),
    func("FFI.new",            zErrFunc(ffi.ffiNew)),
    func("FFI.unbindObjPtr",   zErrFunc(ffi.ffiUnbindObjPtr)),
};

const NameValue = struct { []const u8, cy.Value };
var vars: [7]NameValue = undefined;
pub fn varLoader(_: ?*C.VM, v: C.VarInfo, out: [*c]C.Value) callconv(.C) bool {
    const name = C.fromStr(v.name);
    if (std.mem.eql(u8, vars[v.idx].@"0", name)) {
        out.* = vars[v.idx].@"1".val;
        return true;
    }
    return false;
}

const htype = C.hostTypeEntry;
pub const types = [_]C.HostTypeEntry{
    htype("File",         C.CUSTOM_TYPE(&fs.FileT, null, fs.fileFinalizer)),
    htype("Dir",          C.CUSTOM_TYPE(&fs.DirT, null, fs.dirFinalizer)),
    htype("DirIterator",  C.CUSTOM_TYPE(&fs.DirIterT, fs.dirIterGetChildren, fs.dirIterFinalizer)),
    htype("FFI",          C.CUSTOM_TYPE(&ffi.FFIT, ffi.ffiGetChildren, ffi.ffiFinalizer)),
};

pub fn onTypeLoad(vm_: ?*C.VM, mod: C.Sym) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    zPostTypeLoad(vm.compiler, mod) catch |err| {
        cy.panicFmt("os module: {}", .{err});
    };
}

pub fn zPostTypeLoad(c: *cy.Compiler, mod: C.Sym) !void {
    vars[0] = .{ "cpu", try cy.heap.allocString(c.vm, @tagName(builtin.cpu.arch)) };
    if (builtin.cpu.arch.endian() == .little) {
        vars[1] = .{ "endian", cy.Value.initSymbol(@intFromEnum(Symbol.little)) };
    } else {
        vars[1] = .{ "endian", cy.Value.initSymbol(@intFromEnum(Symbol.big)) };
    }
    if (cy.hasStdFiles) {
        const stderr = try fs.allocFile(c.vm, std.io.getStdErr().handle);
        stderr.castHostObject(*fs.File).closeOnFree = false;
        vars[2] = .{ "stderr", stderr };
        const stdin = try fs.allocFile(c.vm, std.io.getStdIn().handle);
        stdin.castHostObject(*fs.File).closeOnFree = false;
        vars[3] = .{ "stdin", stdin };
        const stdout = try fs.allocFile(c.vm, std.io.getStdOut().handle);
        stdout.castHostObject(*fs.File).closeOnFree = false;
        vars[4] = .{ "stdout", stdout };
    } else {
        const stderr = try fs.allocFile(c.vm, 0);
        stderr.castHostObject(*fs.File).closeOnFree = false;
        stderr.castHostObject(*fs.File).closed = true;
        vars[2] = .{ "stderr", stderr };
        const stdin = try fs.allocFile(c.vm, 0);
        stdin.castHostObject(*fs.File).closeOnFree = false;
        stdin.castHostObject(*fs.File).closed = true;
        vars[3] = .{ "stdin", stdin };
        const stdout = try fs.allocFile(c.vm, 0);
        stdout.castHostObject(*fs.File).closeOnFree = false;
        stdout.castHostObject(*fs.File).closed = true;
        vars[4] = .{ "stdout", stdout };
    }
    vars[5] = .{ "system", try cy.heap.allocString(c.vm, @tagName(builtin.os.tag)) };
    
    if (comptime std.simd.suggestVectorLength(u8)) |VecSize| {
        vars[6] = .{ "vecBitSize", cy.Value.initI32(VecSize * 8) };
    } else {
        vars[6] = .{ "vecBitSize", cy.Value.initI32(0) };
    }

    const sym = cy.Sym.fromC(mod);
    const chunkMod = sym.getMod().?;
    CArrayT = chunkMod.getSym("CArray").?.cast(.object_t).type;
    CDimArrayT = chunkMod.getSym("CDimArray").?.cast(.object_t).type;
    nextUniqId = 1;
}

pub fn onLoad(vm_: ?*C.VM, mod: C.Sym) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    zPostLoad(vm.compiler, mod) catch |err| {
        cy.panicFmt("os module: {}", .{err});
    };
}

fn zPostLoad(self: *cy.Compiler, mod: C.Sym) anyerror!void {
    const b = bindings.ModuleBuilder.init(self, cy.Sym.fromC(mod));
    _ = b;

    // Free vars since they are managed by the module now.
    log.tracev("os post load", .{});
    for (vars) |entry| {
        cy.arc.release(self.vm, entry.@"1");
    }
}

fn openDir(vm: *cy.VM, args: [*]const Value, nargs: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    return openDir2(vm, &[_]Value{ args[0], Value.False }, nargs);
}

fn openDir2(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    const iterable = args[1].asBool();
    const dir = try std.fs.cwd().openDir(path, .{
        .iterate = iterable,
    });
    const fd = dir.fd;
    return fs.allocDir(vm, fd, iterable) catch fatal();
}

fn removeDir(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    try std.fs.cwd().deleteDir(path);
    return Value.Void;
}

fn copyFile(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const src = args[0].asString();
    const alloc = vm.alloc;
    const srcDupe = alloc.dupe(u8, src) catch fatal();
    defer alloc.free(srcDupe);
    const dst = args[1].asString();
    try std.fs.cwd().copyFile(srcDupe, std.fs.cwd(), dst, .{});
    return Value.Void;
}

fn removeFile(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    try std.fs.cwd().deleteFile(path);
    return Value.Void;
}

fn createDir(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    try std.fs.cwd().makeDir(path);
    return Value.Void;
}

fn createFile(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    const truncate = args[1].asBool();
    const file = try std.fs.cwd().createFile(path, .{ .truncate = truncate });
    return fs.allocFile(vm, file.handle) catch fatal();
}

pub fn access(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");

    const path = args[0].asString();

    const mode: Symbol = @enumFromInt(args[1].asSymbolId());
    const zmode: std.fs.File.OpenMode = switch (mode) {
        .read => .read_only,
        .write => .write_only,
        .readWrite => .read_write,
        else => {
            return error.InvalidArgument;
        }
    };
    try std.fs.cwd().access(path, .{ .mode = zmode });
    return Value.Void;
}

fn openFile(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    const mode: Symbol = @enumFromInt(args[1].asSymbolId());
    const zmode: std.fs.File.OpenMode = switch (mode) {
        .read => .read_only,
        .write => .write_only,
        .readWrite => .read_write,
        else => {
            return error.InvalidArgument;
        }
    };
    const file = try std.fs.cwd().openFile(path, .{ .mode = zmode });
    return fs.allocFile(vm, file.handle);
}

fn parseArgs(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");

    const list = args[0].asHeapObject().list.items();

    // Build options map.
    const OptionType = enum {
        string,
        float,
        bool,
    };
    const Option = struct {
        name: Value,
        type: OptionType,
        default: Value,

        found: bool,
    };
    var optionMap: std.StringHashMapUnmanaged(Option) = .{};
    defer optionMap.deinit(vm.alloc);
    for (list) |opt| {
        if (opt.isObjectType(bt.Table)) {
            const entry = opt.asHeapObject().table.map();
            const name = entry.getByString("name") orelse return error.InvalidArgument;
            if (!name.isString()) {
                return error.InvalidArgument;
            }
            const entryType = entry.getByString("type") orelse return error.InvalidArgument;
            if (!entryType.isObjectType(bt.MetaType)) {
                return error.InvalidArgument;
            }
            var optType: OptionType = undefined;
            switch (entryType.asHeapObject().metatype.type) {
                bt.String => {
                    optType = .string;
                },
                bt.Float => {
                    optType = .float;
                },
                bt.Boolean => {
                    optType = .bool;
                },
                else => {
                    return error.InvalidArgument;
                },
            }
            const default = entry.getByString("default") orelse b: {
                vm.retain(vm.emptyString);
                break :b vm.emptyString;
            };
            try optionMap.put(vm.alloc, name.asString(), .{
                .name = name,
                .type = optType,
                .default = default,
                .found = false,
            });
        } else {
            return error.InvalidArgument;
        }
    }

    const res = try vm.allocTable();
    const map = res.asHeapObject().table.map();

    var iter = try std.process.argsWithAllocator(vm.alloc);
    defer iter.deinit();
    const rest = try vm.allocEmptyListDyn();
    const restList = rest.asHeapObject().list.getList();
    while (iter.next()) |arg| {
        if (arg[0] == '-') {
            const optName = arg[1..];
            if (optionMap.getPtr(optName)) |opt| {
                if (opt.found) {
                    continue;
                }
                switch (opt.type) {
                    .string => {
                        if (iter.next()) |nextArg| {
                            const val = try vm.allocString(nextArg);
                            vm.retain(opt.name);
                            try map.put(vm.alloc, opt.name, val);
                            opt.found = true;
                        } else {
                            return error.InvalidArgument;
                        }
                    },
                    .float => {
                        if (iter.next()) |nextArg| {
                            const num = std.fmt.parseFloat(f64, nextArg) catch {
                                return error.InvalidArgument;
                            };
                            vm.retain(opt.name);
                            try map.put(vm.alloc, opt.name, Value.initF64(num));
                            opt.found = true;
                        } else {
                            return error.InvalidArgument;
                        }
                    },
                    .bool => {
                        vm.retain(opt.name);
                        try map.put(vm.alloc, opt.name, Value.True);
                        opt.found = true;
                    }
                }
                continue;
            }
        }
        const str = try vm.allocString(arg);
        try restList.append(vm.alloc, str);
    }

    // Fill missing with defaults.
    var optIter = optionMap.valueIterator();
    while (optIter.next()) |opt| {
        if (!opt.*.found) {
            vm.retain(opt.*.name);
            vm.retain(opt.*.default);
            try map.put(vm.alloc, opt.*.name, opt.*.default);
        }
    }

    try map.put(vm.alloc, try vm.retainOrAllocAstring("rest"), rest);
    return res;
}

fn osArgs(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    var iter = try std.process.argsWithAllocator(vm.alloc);
    defer iter.deinit();
    const listv = try vm.allocEmptyListDyn();
    const listo = listv.asHeapObject();
    while (iter.next()) |arg| {
        const str = try vm.allocString(arg);
        try listo.list.append(vm.alloc, str);
    }
    return listv;
}

pub fn cwd(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const res = try std.process.getCwdAlloc(vm.alloc);
    defer vm.alloc.free(res);
    // TODO: Use allocOwnedString
    return vm.allocString(res);
}

pub fn exePath(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = try std.fs.selfExePathAlloc(vm.alloc);
    defer vm.alloc.free(path);
    // TODO: Use allocOwnedString
    return vm.allocString(path);
}

const StringNone = cy.builtins.StringNone;
const StringSome = cy.builtins.StringSome;

pub fn getEnv(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.prepPanic("Unsupported.");
    const key = args[0].asString();
    const res = std.posix.getenv(key) orelse return StringNone(vm);
    return StringSome(vm, try vm.allocString(res));
}

pub fn getEnvAll(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.prepPanic("Unsupported.");
    var env = try std.process.getEnvMap(vm.alloc);
    defer env.deinit();

    const map = try vm.allocEmptyMap();
    var iter = env.iterator();
    while (iter.next()) |entry| {
        const key = try vm.allocString(entry.key_ptr.*);
        const val = try vm.allocString(entry.value_ptr.*);
        defer {
            vm.release(key);
            vm.release(val);
        }
        try map.asHeapObject().map.set(vm, key, val);
    }
    return map;
}

pub fn free(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const ptr = args[0].asHeapObject().pointer.ptr;
    std.c.free(ptr);
    return Value.Void;
}

pub fn malloc(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const size: usize = @intCast(args[0].asInteger());
    const ptr = std.c.malloc(size);
    return cy.heap.allocPointer(vm, ptr);
}

fn cstr(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const bytes = try vm.getOrBufPrintValueRawStr(&cy.tempBuf, args[0]);
    const new: [*]u8 = @ptrCast(std.c.malloc(bytes.len + 1));
    @memcpy(new[0..bytes.len], bytes);
    new[bytes.len] = 0;
    return cy.heap.allocPointer(vm, new);
}

pub fn now(_: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    const i = try std.time.Instant.now();
    if (builtin.os.tag == .windows) {
        const qpf = std.os.windows.QueryPerformanceFrequency();

        const common_qpf = 10_000_000;
        if (qpf == common_qpf) {
            const ns = i.timestamp * (std.time.ns_per_s / common_qpf);
            return Value.initF64(@as(f64, @floatFromInt(ns)) / @as(f64, std.time.ns_per_s));
        }

        // Convert to ns using fixed point.
        const scale = @as(u64, std.time.ns_per_s << 32) / @as(u32, @intCast(qpf));
        const ns = (@as(u96, i.timestamp) * scale) >> 32;
        return Value.initF64(@as(f64, @floatFromInt(ns)) / @as(f64, std.time.ns_per_s));
    }

    // WASI timestamps are directly in nanoseconds
    if (builtin.os.tag == .wasi) {
        return Value.initF64(@as(f64, @floatFromInt(i.timestamp)) / @as(f64, std.time.ns_per_s));
    }

    const seconds = @as(u64, @intCast(i.timestamp.tv_sec));
    const ns = (seconds * std.time.ns_per_s) + @as(u32, @intCast(i.timestamp.tv_nsec));
    return Value.initF64(@as(f64, @floatFromInt(ns)) / @as(f64, std.time.ns_per_s));
}

pub fn milliTime(_: *cy.VM, _: [*]const Value, _: u8) Value {
    return Value.initF64(@floatFromInt(stdx.time.getMilliTimestamp()));
}

pub fn dirName(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    if (std.fs.path.dirname(path)) |res| {
        return StringSome(vm, try vm.allocString(res));
    } else {
        return StringNone(vm);
    }
}

pub fn realPath(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    const res = try std.fs.cwd().realpathAlloc(vm.alloc, path);
    defer vm.alloc.free(res);
    // TODO: Use allocOwnedString.
    return vm.allocString(res);
}

pub fn setEnv(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.prepPanic("Unsupported.");
    const key = args[0].asString();
    const keyz = try vm.alloc.dupeZ(u8, key);
    defer vm.alloc.free(keyz);

    const value = args[1].asString();
    const valuez = try vm.alloc.dupeZ(u8, value);
    defer vm.alloc.free(valuez);
    _ = setenv(keyz, valuez, 1);
    return Value.Void;
}
pub extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;

pub fn sleep(_: *cy.VM, args: [*]const Value, _: u8) Value {
    if (builtin.os.tag == .windows) {
        const ms: u32 = @intFromFloat(args[0].asF64());
        std.os.windows.kernel32.Sleep(ms);
    } else {
        const ms = args[0].asF64();
        const secs: u64 = @intFromFloat(@divFloor(ms, 1000));
        const nsecs: u64 = @intFromFloat(1e6 * (std.math.mod(f64, ms, 1000) catch cy.fatal()));
        if (cy.isWasm) {
            hostSleep(secs, nsecs);
        } else {
            std.posix.nanosleep(secs, nsecs);
        }
    }
    return Value.Void;
}

extern fn hostSleep(secs: u64, nsecs: u64) void;

pub fn unsetEnv(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.prepPanic("Unsupported.");
    const key = args[0].asString();
    const keyz = vm.alloc.dupeZ(u8, key) catch cy.fatal();
    defer vm.alloc.free(keyz);
    _ = unsetenv(keyz);
    return Value.Void;
}
pub extern "c" fn unsetenv(name: [*:0]const u8) c_int;

fn newFFI(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    _ = args;
    if (!cy.hasFFI) return vm.prepPanic("Unsupported.");
    return ffi.allocFFI(vm) catch fatal();
}

pub fn bindLib(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasFFI) return vm.prepPanic("Unsupported.");

    return @call(.never_inline, ffi.ffiBindLib, .{vm, args, .{}});
}

pub fn bindLibExt(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasFFI) return vm.prepPanic("Unsupported.");

    var configV = args[2];
    const gen_table = try vm.retainOrAllocAstring("gen_table");
    defer vm.release(gen_table);
    var config: ffi.BindLibConfig = .{};
    const val = configV.asHeapObject().table.get(gen_table) orelse Value.False;
    if (val.isTrue()) {
        config.gen_table = true;
    }
    return @call(.never_inline, ffi.ffiBindLib, .{vm, args, config});
}

pub extern fn hostFileWrite(fid: u32, str: [*]const u8, strLen: usize) void;

fn cacheUrl(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const url = args[0].asString();

    const specGroup = try cache.getSpecHashGroup(vm.alloc, url);
    defer specGroup.deinit(vm.alloc);

    if (vm.config.reload) {
        try specGroup.markEntryBySpecForRemoval(url);
    } else {
        // First check local cache.
        if (try specGroup.findEntryBySpec(url)) |entry| {
            const path = try cache.allocSpecFilePath(vm.alloc, entry);
            defer vm.alloc.free(path);
            return vm.allocString(path);
        }
    }

    const resp = try http.get(vm.alloc, vm.httpClient, url);
    defer vm.alloc.free(resp.body);
    if (resp.status != .ok) {
        log.tracev("cacheUrl response status: {}", .{resp.status});
        return rt.prepThrowError(vm, .UnknownError);
    } else {
        const entry = try cache.saveNewSpecFile(vm.alloc, specGroup, url, resp.body);
        defer entry.deinit(vm.alloc);
        const path = try cache.allocSpecFilePath(vm.alloc, entry);
        defer vm.alloc.free(path);
        return vm.allocString(path);
    }
}

pub fn execCmd(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");

    const obj = args[0].asHeapObject();
    var buf: std.ArrayListUnmanaged([]const u8) = .{};
    defer {
        for (buf.items) |arg| {
            vm.alloc.free(arg);
        }
        buf.deinit(vm.alloc);
    }
    for (obj.list.items()) |arg| {
        const str = try vm.allocValueStr(arg);
        try buf.append(vm.alloc, str);
    }

    const res = try std.ChildProcess.run(.{
        .allocator = vm.alloc,
        .argv = buf.items,
        .max_output_bytes = 1024 * 1024 * 10,
    });

    const map = try vm.allocEmptyMap();
    const outKey = try vm.retainOrAllocAstring("out");
    const errKey = try vm.retainOrAllocAstring("err");
    defer {
        vm.release(outKey);
        vm.release(errKey);
    }

    // TODO: Use allocOwnedString
    defer vm.alloc.free(res.stdout);
    const out = try vm.allocString(res.stdout);
    defer vm.release(out);
    try map.asHeapObject().map.set(vm, outKey, out);
    // TODO: Use allocOwnedString
    defer vm.alloc.free(res.stderr);
    const err = try vm.allocString(res.stderr);
    defer vm.release(err);
    try map.asHeapObject().map.set(vm, errKey, err);
    if (res.term == .Exited) {
        const exitedKey = try vm.retainOrAllocAstring("exited");
        defer vm.release(exitedKey);
        try map.asHeapObject().map.set(vm, exitedKey, Value.initInt(@intCast(res.term.Exited)));
    }
    return map;
}

pub fn exit(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const status: u8 = @intCast(args[0].asInteger());
    std.posix.exit(status);
}

pub fn fetchUrl(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const url = args[0].asString();
    if (cy.isWasm) {
        hostFetchUrl(url.ptr, url.len);
        return Value.None;
    } else {
        const resp = try http.get(vm.alloc, vm.httpClient, url);
        defer vm.alloc.free(resp.body);
        // TODO: Use allocOwnedString
        return vm.allocArray(resp.body);
    }
}

extern fn hostFetchUrl(url: [*]const u8, urlLen: usize) void;

pub fn readLine(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    const input = try std.io.getStdIn().reader().readUntilDelimiterAlloc(vm.alloc, '\n', 10e8);
    defer vm.alloc.free(input);
    // TODO: Use allocOwnedString
    return vm.allocString(input);
}

pub fn readAll(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    const input = try std.io.getStdIn().readToEndAlloc(vm.alloc, 10e8);
    defer vm.alloc.free(input);
    // TODO: Use allocOwnString.
    return vm.allocString(input);
}

pub fn readFile(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const path = args[0].asString();
    const content = try std.fs.cwd().readFileAlloc(vm.alloc, path, 10e8);
    defer vm.alloc.free(content);
    // TODO: Use allocOwnedString.
    return vm.allocString(content);
}

pub fn writeFile(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    const content = try vm.getOrBufPrintValueRawStr(&cy.tempBuf, args[1]);
    try std.fs.cwd().writeFile(path, content);
    return Value.Void;
}