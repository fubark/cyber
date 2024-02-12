const std = @import("std");
const stdx = @import("stdx");
const fatal = cy.fatal;
const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const cc = @import("../capi.zig");
const vmc = cy.vmc;
const rt = cy.rt;
const Value = cy.Value;
const fmt = @import("../fmt.zig");
const bindings = @import("../builtins/bindings.zig");
const builtins = @import("../builtins/builtins.zig");
const prepThrowZError = builtins.prepThrowZError;
const zErrFunc = builtins.zErrFunc;
const zErrFunc2 = builtins.zErrFunc2;
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
pub fn funcLoader(_: ?*cc.VM, func: cc.FuncInfo, out_: [*c]cc.FuncResult) callconv(.C) bool {
    const out: *cc.FuncResult = out_;
    const name = cc.strSlice(func.name);
    if (std.mem.eql(u8, funcs[func.idx].@"0", name)) {
        out.ptr = @ptrCast(funcs[func.idx].@"1");
        return true;
    }
    return false;
}

const NameFunc = struct { []const u8, cy.ZHostFuncFn };
const funcs = [_]NameFunc{
    // Top level
    .{"access",         zErrFunc2(access)},
    .{"args",           zErrFunc2(osArgs)},
    .{"cacheUrl",       zErrFunc2(cacheUrl)},
    .{"copyFile",       zErrFunc(copyFile)},
    .{"createDir",      zErrFunc(createDir)},
    .{"createFile",     zErrFunc2(createFile)},
    .{"cstr",           zErrFunc2(cstr)},
    .{"cwd",            zErrFunc2(cwd)},
    .{"dirName",        zErrFunc2(dirName)},
    .{"execCmd",        zErrFunc2(execCmd)},
    .{"exePath",        zErrFunc2(exePath)},
    .{"exit",           exit},
    .{"fetchUrl",       zErrFunc2(fetchUrl)},
    .{"free",           free},
    .{"getEnv",         zErrFunc2(getEnv)},
    .{"getEnvAll",      zErrFunc2(getEnvAll)},
    .{"malloc",         zErrFunc(malloc)},
    .{"milliTime",      milliTime},
    .{"newFFI",         newFFI},
    .{"now",            zErrFunc2(now)},
    .{"openDir",        zErrFunc2(openDir)},
    .{"openDir",        zErrFunc2(openDir2)},
    .{"openFile",       zErrFunc2(openFile)},
    .{"parseArgs",      zErrFunc2(parseArgs)},
    .{"readAll",        zErrFunc2(readAll)},
    .{"readFile",       zErrFunc2(readFile)},
    .{"readLine",       zErrFunc2(readLine)},
    .{"realPath",       zErrFunc2(realPath)},
    .{"removeDir",      zErrFunc(removeDir)},
    .{"removeFile",     zErrFunc(removeFile)},
    .{"setEnv",         zErrFunc(setEnv)},
    .{"sleep",          sleep},
    .{"unsetEnv",       unsetEnv},
    .{"writeFile",      zErrFunc2(writeFile)},

    // File
    .{"close",          fs.fileClose},
    .{"iterator",       zErrFunc2(fs.fileIterator)},
    .{"next",           zErrFunc2(fs.fileNext)},
    .{"read",           zErrFunc2(fs.fileRead)},
    .{"readAll",        zErrFunc2(fs.fileReadAll)},
    .{"seek",           zErrFunc2(fs.fileSeek)},
    .{"seekFromCur",    zErrFunc2(fs.fileSeekFromCur)},
    .{"seekFromEnd",    zErrFunc2(fs.fileSeekFromEnd)},
    .{"stat",           zErrFunc2(fs.fileOrDirStat)},
    .{"streamLines",    zErrFunc2(fs.fileStreamLines)},
    .{"streamLines",    zErrFunc2(fs.fileStreamLines1)},
    .{"write",          zErrFunc2(fs.fileWrite)},

    // Dir
    .{"iterator",   fs.dirIterator},
    .{"stat",       zErrFunc2(fs.fileOrDirStat)},
    .{"walk",       fs.dirWalk},

    // DirIterator
    .{"next", zErrFunc2(fs.dirIteratorNext)},

    // FFI
    .{"bindCallback",   zErrFunc(ffi.ffiBindCallback)},
    .{"bindLib",        zErrFunc2(bindLib)},
    .{"bindLib",        zErrFunc2(bindLibExt)},
    .{"bindObjPtr",     zErrFunc2(ffi.ffiBindObjPtr)},
    .{"cbind",          zErrFunc(ffi.ffiCbind)},
    .{"cfunc",          zErrFunc2(ffi.ffiCfunc)},
    .{"new",            zErrFunc2(ffi.ffiNew)},
    .{"unbindObjPtr",   zErrFunc2(ffi.ffiUnbindObjPtr)},
};

const NameValue = struct { []const u8, cy.Value };
var vars: [7]NameValue = undefined;
pub fn varLoader(_: ?*cc.VM, v: cc.VarInfo, out: [*c]cc.Value) callconv(.C) bool {
    const name = cc.strSlice(v.name);
    if (std.mem.eql(u8, vars[v.idx].@"0", name)) {
        out.* = vars[v.idx].@"1".val;
        return true;
    }
    return false;
}

const NameType = struct { []const u8, *cy.TypeId, cc.ObjectGetChildrenFn, cc.ObjectFinalizerFn };
const types = [_]NameType{
    .{"File", &fs.FileT, null, fs.fileFinalizer },
    .{"Dir", &fs.DirT, null, fs.dirFinalizer },
    .{"DirIterator", &fs.DirIterT, fs.dirIteratorGetChildren, fs.dirIteratorFinalizer },
    .{"FFI", &ffi.FFIT, ffi.ffiGetChildren, ffi.ffiFinalizer },
};

pub fn typeLoader(_: ?*cc.VM, info: cc.TypeInfo, out_: [*c]cc.TypeResult) callconv(.C) bool {
    const out: *cc.TypeResult = out_;
    const name = cc.strSlice(info.name);
    if (std.mem.eql(u8, types[info.idx].@"0", name)) {
        out.type = cc.TypeKindObject;
        out.data.object = .{
            .outTypeId = types[info.idx].@"1",
            .getChildren = types[info.idx].@"2",
            .finalizer = types[info.idx].@"3",
        };
        return true;
    }
    return false;
}

pub fn onTypeLoad(vm_: ?*cc.VM, mod: cc.ApiModule) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    zPostTypeLoad(vm.compiler, mod) catch |err| {
        cy.panicFmt("os module: {}", .{err});
    };
}

pub fn zPostTypeLoad(c: *cy.VMcompiler, mod: cc.ApiModule) !void {
    vars[0] = .{ "cpu", try cy.heap.allocStringOrFail(c.vm, @tagName(builtin.cpu.arch)) };
    if (builtin.cpu.arch.endian() == .Little) {
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
    vars[5] = .{ "system", try cy.heap.allocStringOrFail(c.vm, @tagName(builtin.os.tag)) };
    
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        vars[6] = .{ "vecBitSize", cy.Value.initI32(VecSize * 8) };
    } else {
        vars[6] = .{ "vecBitSize", cy.Value.initI32(0) };
    }

    const sym: *cy.Sym = @ptrCast(@alignCast(mod.sym));
    const chunkMod = sym.getMod().?;
    CArrayT = chunkMod.getSym("CArray").?.cast(.object_t).type;
    CDimArrayT = chunkMod.getSym("CDimArray").?.cast(.object_t).type;
    nextUniqId = 1;
}

pub fn onLoad(vm_: ?*cc.VM, mod: cc.ApiModule) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    zPostLoad(vm.compiler, mod) catch |err| {
        cy.panicFmt("os module: {}", .{err});
    };
}

fn zPostLoad(self: *cy.VMcompiler, mod: cc.ApiModule) anyerror!void {
    const b = bindings.ModuleBuilder.init(self, @ptrCast(@alignCast(mod.sym)));
    _ = b;

    // Free vars since they are managed by the module now.
    log.tracev("os post load", .{});
    for (vars) |entry| {
        cy.arc.release(self.vm, entry.@"1");
    }
}

fn openDir(vm: *cy.VM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    return openDir2(vm, &[_]Value{ args[0], Value.False }, nargs);
}

fn openDir2(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    const iterable = args[1].asBool();
    var fd: std.os.fd_t = undefined;
    if (iterable) {
        const dir = try std.fs.cwd().openIterableDir(path, .{});
        fd = dir.dir.fd;
    } else {
        const dir = try std.fs.cwd().openDir(path, .{});
        fd = dir.fd;
    }
    return fs.allocDir(vm, fd, iterable) catch fatal();
}

fn removeDir(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = args[0].asString();
    try std.fs.cwd().deleteDir(path);
    return Value.None;
}

fn copyFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const src = args[0].asString();
    const alloc = vm.allocator();
    const srcDupe = alloc.dupe(u8, src) catch fatal();
    defer alloc.free(srcDupe);
    const dst = args[1].asString();
    try std.fs.cwd().copyFile(srcDupe, std.fs.cwd(), dst, .{});
    return Value.None;
}

fn removeFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = args[0].asString();
    try std.fs.cwd().deleteFile(path);
    return Value.None;
}

fn createDir(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = args[0].asString();
    try std.fs.cwd().makeDir(path);
    return Value.None;
}

fn createFile(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
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
    return Value.None;
}

fn openFile(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
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
        if (opt.isObjectType(bt.Map)) {
            const entry = opt.asHeapObject().map.map();
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
            const default = entry.getByString("default") orelse Value.None;
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

    const res = try vm.allocEmptyMap();
    const map = res.asHeapObject().map.map();

    var iter = try std.process.argsWithAllocator(vm.alloc);
    defer iter.deinit();
    const rest = try vm.allocEmptyList();
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
                            const val = try vm.allocStringOrFail(nextArg);
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
        const str = try vm.allocStringOrFail(arg);
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

fn osArgs(vm: *cy.VM, _: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    var iter = try std.process.argsWithAllocator(vm.alloc);
    defer iter.deinit();
    const listv = try vm.allocEmptyList();
    const listo = listv.asHeapObject();
    while (iter.next()) |arg| {
        const str = try vm.allocStringOrFail(arg);
        try listo.list.append(vm.alloc, str);
    }
    return listv;
}

pub fn cwd(vm: *cy.VM, _: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const res = try std.process.getCwdAlloc(vm.alloc);
    defer vm.alloc.free(res);
    // TODO: Use allocOwnedString
    return vm.allocStringOrFail(res);
}

pub fn exePath(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = try std.fs.selfExePathAlloc(vm.alloc);
    defer vm.alloc.free(path);
    // TODO: Use allocOwnedString
    return vm.allocStringOrFail(path);
}

pub fn getEnv(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.prepPanic("Unsupported.");
    const key = args[0].asString();
    const res = std.os.getenv(key) orelse return Value.None;
    return vm.allocStringOrFail(res);
}

pub fn getEnvAll(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.prepPanic("Unsupported.");
    var env = try std.process.getEnvMap(vm.alloc);
    defer env.deinit();

    const map = try vm.allocEmptyMap();
    var iter = env.iterator();
    while (iter.next()) |entry| {
        const key = try vm.allocStringOrFail(entry.key_ptr.*);
        const val = try vm.allocStringOrFail(entry.value_ptr.*);
        defer {
            vm.release(key);
            vm.release(val);
        }
        try map.asHeapObject().map.set(vm, key, val);
    }
    return map;
}

pub fn free(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const ptr = args[0].asHeapObject().pointer.ptr;
    std.c.free(ptr);
    return Value.None;
}

pub fn malloc(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const size: usize = @intCast(args[0].asInteger());
    const ptr = std.c.malloc(size);
    return cy.heap.allocPointer(vm.internal(), ptr);
}

fn cstr(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
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
    if (builtin.os.tag == .wasi and !builtin.link_libc) {
        return Value.initF64(@as(f64, @floatFromInt(i.timestamp)) / @as(f64, std.time.ns_per_s));
    }

    const seconds = @as(u64, @intCast(i.timestamp.tv_sec));
    const ns = (seconds * std.time.ns_per_s) + @as(u32, @intCast(i.timestamp.tv_nsec));
    return Value.initF64(@as(f64, @floatFromInt(ns)) / @as(f64, std.time.ns_per_s));
}

pub fn milliTime(_: *cy.VM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.initF64(@floatFromInt(stdx.time.getMilliTimestamp()));
}

pub fn dirName(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    if (std.fs.path.dirname(path)) |res| {
        return vm.allocStringOrFail(res);
    } else {
        return Value.None;
    }
}

pub fn realPath(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    const res = try std.fs.cwd().realpathAlloc(vm.alloc, path);
    defer vm.alloc.free(res);
    // TODO: Use allocOwnedString.
    return vm.allocStringOrFail(res);
}

pub fn setEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.returnPanic("Unsupported.");
    const key = args[0].asString();
    const keyz = try vm.allocator().dupeZ(u8, key);
    defer vm.allocator().free(keyz);

    const value = args[1].asString();
    const valuez = try vm.allocator().dupeZ(u8, value);
    defer vm.allocator().free(valuez);
    _ = setenv(keyz, valuez, 1);
    return Value.None;
}
pub extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;

pub fn sleep(_: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
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
            std.os.nanosleep(secs, nsecs);
        }
    }
    return Value.None;
}

extern fn hostSleep(secs: u64, nsecs: u64) void;

pub fn unsetEnv(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.prepPanic("Unsupported.");
    const key = args[0].asString();
    const keyz = vm.alloc.dupeZ(u8, key) catch cy.fatal();
    defer vm.alloc.free(keyz);
    _ = unsetenv(keyz);
    return Value.None;
}
pub extern "c" fn unsetenv(name: [*:0]const u8) c_int;

fn newFFI(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    _ = args;
    if (!cy.hasFFI) return vm.prepPanic("Unsupported.");
    return ffi.allocFFI(vm) catch fatal();
}

pub fn bindLib(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasFFI) return vm.prepPanic("Unsupported.");

    return @call(.never_inline, ffi.ffiBindLib, .{vm, args, .{}});
}

pub fn bindLibExt(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasFFI) return vm.prepPanic("Unsupported.");

    var configV = args[2];
    const genMapV = try vm.retainOrAllocAstring("genMap");
    defer vm.release(genMapV);
    var config: ffi.BindLibConfig = .{};
    const val = configV.asHeapObject().map.map().get(genMapV) orelse Value.False;
    if (val.isTrue()) {
        config.genMap = true;
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
            return vm.allocStringOrFail(path);
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
        return vm.allocStringOrFail(path);
    }
}

pub fn execCmd(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
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

    const res = try std.ChildProcess.exec(.{
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
    const out = try vm.allocStringInternOrArray(res.stdout);
    defer vm.release(out);
    try map.asHeapObject().map.set(vm, outKey, out);
    // TODO: Use allocOwnedString
    defer vm.alloc.free(res.stderr);
    const err = try vm.allocStringInternOrArray(res.stderr);
    defer vm.release(err);
    try map.asHeapObject().map.set(vm, errKey, err);
    if (res.term == .Exited) {
        const exitedKey = try vm.retainOrAllocAstring("exited");
        defer vm.release(exitedKey);
        try map.asHeapObject().map.set(vm, exitedKey, Value.initInt(@intCast(res.term.Exited)));
    }
    return map;
}

pub fn exit(_: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const status: u8 = @intCast(args[0].asInteger());
    std.os.exit(status);
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
    return vm.allocStringOrFail(input);
}

pub fn readAll(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    const input = try std.io.getStdIn().readToEndAlloc(vm.alloc, 10e8);
    defer vm.alloc.free(input);
    // TODO: Use allocOwnString.
    return vm.allocStringOrFail(input);
}

pub fn readFile(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");

    const path = args[0].asString();
    const content = try std.fs.cwd().readFileAlloc(vm.alloc, path, 10e8);
    defer vm.alloc.free(content);
    // TODO: Use allocOwnedString.
    return vm.allocStringOrFail(content);
}

pub fn writeFile(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasStdFiles) return vm.prepPanic("Unsupported.");
    const path = args[0].asString();
    const content = try vm.getOrBufPrintValueRawStr(&cy.tempBuf, args[1]);
    try std.fs.cwd().writeFile(path, content);
    return Value.None;
}