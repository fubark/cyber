const std = @import("std");
const stdx = @import("stdx");
const fatal = cy.fatal;
const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const cc = @import("../clib.zig");
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

pub var CFuncT: cy.TypeId = undefined;
pub var CStructT: cy.TypeId = undefined;
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
    .{"access",         zErrFunc(access)},
    .{"args",           zErrFunc(osArgs)},
    .{"cacheUrl",       cacheUrl},
    .{"copyFile",       zErrFunc(copyFile)},
    .{"createDir",      zErrFunc(createDir)},
    .{"createFile",     zErrFunc(createFile)},
    .{"cstr",           zErrFunc2(cstr)},
    .{"cwd",            zErrFunc(cwd)},
    .{"dirName",        zErrFunc(dirName)},
    .{"execCmd",        zErrFunc(execCmd)},
    .{"exePath",        zErrFunc(exePath)},
    .{"exit",           exit},
    .{"fetchUrl",       zErrFunc2(fetchUrl)},
    .{"free",           free},
    .{"fromCstr",       zErrFunc2(fromCstr)},
    .{"getEnv",         zErrFunc(getEnv)},
    .{"getEnvAll",      zErrFunc(getEnvAll)},
    .{"getAllInput",    zErrFunc(getAllInput)},
    .{"getInput",       getInput},
    .{"malloc",         zErrFunc(malloc)},
    .{"milliTime",      milliTime},
    .{"newFFI",         newFFI},
    .{"openDir",        zErrFunc(openDir)},
    .{"openDir",        zErrFunc(openDir2)},
    .{"openFile",       zErrFunc(openFile)},
    .{"parseArgs",      zErrFunc(parseArgs)},
    .{"readFile",       zErrFunc(readFile)},
    .{"realPath",       zErrFunc(realPath)},
    .{"removeDir",      zErrFunc(removeDir)},
    .{"removeFile",     zErrFunc(removeFile)},
    .{"setEnv",         zErrFunc(setEnv)},
    .{"sleep",          sleep},
    .{"unsetEnv",       unsetEnv},
    .{"writeFile",      zErrFunc2(writeFile)},

    // File
    .{"close",          fs.fileClose},
    .{"iterator",       fs.fileIterator},
    .{"next",           zErrFunc2(fs.fileNext)},
    .{"read",           zErrFunc2(fs.fileRead)},
    .{"readToEnd",      zErrFunc2(fs.fileReadToEnd)},
    .{"seek",           fs.fileSeek},
    .{"seekFromCur",    fs.fileSeekFromCur},
    .{"seekFromEnd",    fs.fileSeekFromEnd},
    .{"stat",           fs.fileOrDirStat},
    .{"streamLines",    fs.fileStreamLines},
    .{"streamLines",    fs.fileStreamLines1},
    .{"write",          zErrFunc2(fs.fileWrite)},

    // Dir
    .{"iterator",   fs.dirIterator},
    .{"stat",       fs.fileOrDirStat},
    .{"walk",       fs.dirWalk},

    // DirIterator
    .{"next", fs.dirIteratorNext},

    // FFI
    .{"addCallback",    zErrFunc(ffi.ffiAddCallback)},
    .{"addObjPtr",      zErrFunc2(ffi.ffiAddObjPtr)},
    .{"bindLib",        zErrFunc(bindLib)},
    .{"bindLib",        zErrFunc(bindLibExt)},
    .{"cbind",          zErrFunc(ffi.ffiCbind)},
    .{"cfunc",          zErrFunc2(ffi.ffiCfunc)},
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
    vars[0] = .{ "Root.cpu", try cy.heap.retainOrAllocPreferString(c.vm, @tagName(builtin.cpu.arch)) };
    if (builtin.cpu.arch.endian() == .Little) {
        vars[1] = .{ "Root.endian", cy.Value.initSymbol(@intFromEnum(Symbol.little)) };
    } else {
        vars[1] = .{ "Root.endian", cy.Value.initSymbol(@intFromEnum(Symbol.big)) };
    }
    if (cy.hasStdFiles) {
        const stderr = try fs.allocFile(c.vm, std.io.getStdErr().handle);
        stderr.castHostObject(*fs.File).closeOnFree = false;
        vars[2] = .{ "Root.stderr", stderr };
        const stdin = try fs.allocFile(c.vm, std.io.getStdIn().handle);
        stdin.castHostObject(*fs.File).closeOnFree = false;
        vars[3] = .{ "Root.stdin", stdin };
        const stdout = try fs.allocFile(c.vm, std.io.getStdOut().handle);
        stdout.castHostObject(*fs.File).closeOnFree = false;
        vars[4] = .{ "Root.stdout", stdout };
    } else {
        vars[2] = .{ "Root.stderr", Value.None };
        vars[3] = .{ "Root.stdin", Value.None };
        vars[4] = .{ "Root.stdout", Value.None };
    }
    vars[5] = .{ "Root.system", try cy.heap.retainOrAllocPreferString(c.vm, @tagName(builtin.os.tag)) };
    
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        vars[6] = .{ "Root.vecBitSize", cy.Value.initI32(VecSize * 8) };
    } else {
        vars[6] = .{ "Root.vecBitSize", cy.Value.initI32(0) };
    }

    const sym: *cy.Sym = @ptrCast(@alignCast(mod.sym));
    const chunkMod = sym.getMod().?;
    CFuncT = chunkMod.getSym("CFunc").?.cast(.object).type;
    CStructT = chunkMod.getSym("CStruct").?.cast(.object).type;
    CArrayT = chunkMod.getSym("CArray").?.cast(.object).type;
    CDimArrayT = chunkMod.getSym("CDimArray").?.cast(.object).type;
    nextUniqId = 1;
}

pub fn onLoad(vm_: ?*cc.VM, mod: cc.ApiModule) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    zPostLoad(vm.compiler, mod) catch |err| {
        cy.panicFmt("os module: {}", .{err});
    };
}

fn zPostLoad(self: *cy.VMcompiler, mod: cc.ApiModule) linksection(cy.InitSection) anyerror!void {
    const b = bindings.ModuleBuilder.init(self, @ptrCast(@alignCast(mod.sym)));
    _ = b;

    // Free vars since they are managed by the module now.
    log.tracev("os post load", .{});
    for (vars) |entry| {
        cy.arc.release(self.vm, entry.@"1");
    }
}

fn openDir(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    return openDir2(vm, &[_]Value{ args[0], Value.False }, nargs);
}

fn openDir2(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
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
    return vm.allocDir(fd, iterable) catch fatal();
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

fn createFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = args[0].asString();
    const truncate = args[1].asBool();
    const file = try std.fs.cwd().createFile(path, .{ .truncate = truncate });
    return vm.allocFile(file.handle) catch fatal();
}

pub fn access(vm: *cy.UserVM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");

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

fn openFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
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
    return vm.allocFile(file.handle);
}

fn parseArgs(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const ivm = vm.internal();
    _ = ivm;
    const alloc = vm.allocator();

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
    defer optionMap.deinit(alloc);
    for (list) |opt| {
        if (opt.isObjectType(bt.Map)) {
            const entry = opt.asHeapObject().map.map();
            const name = entry.getByString("name") orelse return prepareThrowSymbol(vm, .InvalidArgument);
            if (!name.isString()) {
                return prepareThrowSymbol(vm, .InvalidArgument);
            }
            const entryType = entry.getByString("type") orelse return prepareThrowSymbol(vm, .InvalidArgument);
            if (!entryType.isObjectType(bt.MetaType)) {
                return prepareThrowSymbol(vm, .InvalidArgument);
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
                    return prepareThrowSymbol(vm, .InvalidArgument);
                },
            }
            const default = entry.getByString("default") orelse Value.None;
            try optionMap.put(alloc, name.asString(), .{
                .name = name,
                .type = optType,
                .default = default,
                .found = false,
            });
        } else {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
    }

    const res = try vm.allocEmptyMap();
    const map = res.asHeapObject().map.map();

    var iter = try std.process.argsWithAllocator(alloc);
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
                            const val = try vm.retainOrAllocString(nextArg);
                            vm.retain(opt.name);
                            try map.put(alloc, opt.name, val);
                            opt.found = true;
                        } else {
                            return prepareThrowSymbol(vm, .InvalidArgument);
                        }
                    },
                    .float => {
                        if (iter.next()) |nextArg| {
                            const num = std.fmt.parseFloat(f64, nextArg) catch {
                                return prepareThrowSymbol(vm, .InvalidArgument);
                            };
                            vm.retain(opt.name);
                            try map.put(alloc, opt.name, Value.initF64(num));
                            opt.found = true;
                        } else {
                            return prepareThrowSymbol(vm, .InvalidArgument);
                        }
                    },
                    .bool => {
                        vm.retain(opt.name);
                        try map.put(alloc, opt.name, Value.True);
                        opt.found = true;
                    }
                }
            }
            continue;
        }
        const str = try vm.allocStringOrByteArray(arg);
        try restList.append(alloc, str);
    }

    // Fill missing with defaults.
    var optIter = optionMap.valueIterator();
    while (optIter.next()) |opt| {
        if (!opt.*.found) {
            vm.retain(opt.*.name);
            vm.retain(opt.*.default);
            try map.put(alloc, opt.*.name, opt.*.default);
        }
    }

    try map.put(alloc, try vm.retainOrAllocAstring("rest"), rest);
    return res;
}

fn osArgs(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const alloc = vm.allocator();
    var iter = try std.process.argsWithAllocator(alloc);
    defer iter.deinit();
    const listv = try vm.allocEmptyList();
    const listo = listv.asHeapObject();
    while (iter.next()) |arg| {
        const str = try vm.allocStringOrByteArray(arg);
        listo.list.append(vm.allocator(), str);
    }
    return listv;
}

pub fn cwd(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const res = try std.process.getCwdAlloc(vm.allocator());
    defer vm.allocator().free(res);
    // TODO: Use allocOwnedString
    return vm.retainOrAllocString(res);
}

pub fn exePath(vm: *cy.UserVM, _: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = try std.fs.selfExePathAlloc(vm.allocator());
    defer vm.allocator().free(path);
    // TODO: Use allocOwnedString
    return vm.retainOrAllocString(path);
}

pub fn getEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.returnPanic("Unsupported.");
    const key = args[0].asString();
    const res = std.os.getenv(key) orelse return Value.None;
    return vm.retainOrAllocString(res);
}

pub fn getEnvAll(vm: *cy.UserVM, _: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.returnPanic("Unsupported.");
    var env = try std.process.getEnvMap(vm.allocator());
    defer env.deinit();

    const map = vm.allocEmptyMap() catch cy.fatal();
    var iter = env.iterator();
    while (iter.next()) |entry| {
        const key = try vm.retainOrAllocString(entry.key_ptr.*);
        const val = try vm.retainOrAllocString(entry.value_ptr.*);
        defer {
            vm.release(key);
            vm.release(val);
        }
        try map.asHeapObject().map.set(vm.internal(), key, val);
    }
    return map;
}

pub fn free(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
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

fn fromCstr(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const bytes = std.mem.span(@as([*:0]const u8, @ptrCast(args[0].asHeapObject().pointer.ptr)));
    return vm.allocArray(bytes);
}

fn cstr(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const bytes = try vm.getOrBufPrintValueRawStr(&cy.tempBuf, args[0]);
    const new: [*]u8 = @ptrCast(std.c.malloc(bytes.len + 1));
    @memcpy(new[0..bytes.len], bytes);
    new[bytes.len] = 0;
    return cy.heap.allocPointer(vm, new);
}

pub fn milliTime(_: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.initF64(@floatFromInt(stdx.time.getMilliTimestamp()));
}

pub fn dirName(vm: *cy.UserVM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = args[0].asString();
    if (std.fs.path.dirname(path)) |res| {
        return vm.retainOrAllocString(res);
    } else {
        return Value.None;
    }
}

pub fn realPath(vm: *cy.UserVM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const path = args[0].asString();
    const res = try std.fs.cwd().realpathAlloc(vm.allocator(), path);
    defer vm.allocator().free(res);
    // TODO: Use allocOwnedString.
    return vm.retainOrAllocString(res);
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

pub fn sleep(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
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

pub fn unsetEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    if (cy.isWasm or builtin.os.tag == .windows) return vm.returnPanic("Unsupported.");
    const key = args[0].asString();
    const keyz = vm.allocator().dupeZ(u8, key) catch cy.fatal();
    defer vm.allocator().free(keyz);
    _ = unsetenv(keyz);
    return Value.None;
}
pub extern "c" fn unsetenv(name: [*:0]const u8) c_int;

fn newFFI(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    _ = args;
    if (!cy.hasFFI) return vm.returnPanic("Unsupported.");
    return ffi.allocFFI(vm.internal()) catch fatal();
}

pub fn bindLib(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasFFI) return vm.returnPanic("Unsupported.");

    return @call(.never_inline, ffi.ffiBindLib, .{vm, args, .{}});
}

pub fn bindLibExt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasFFI) return vm.returnPanic("Unsupported.");

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

fn cacheUrl(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const alloc = vm.allocator();
    const url = args[0].asString();

    const specGroup = cache.getSpecHashGroup(alloc, url) catch cy.fatal();
    defer specGroup.deinit(alloc);

    if (vm.internal().config.reload) {
        specGroup.markEntryBySpecForRemoval(url) catch cy.fatal();
    } else {
        // First check local cache.
        if (specGroup.findEntryBySpec(url) catch cy.fatal()) |entry| {
            const path = cache.allocSpecFilePath(alloc, entry) catch cy.fatal();
            defer alloc.free(path);
            return vm.retainOrAllocString(path) catch cy.fatal();
        }
    }

    const resp = http.get(alloc, vm.internal().httpClient, url) catch |err| {
        log.debug("cacheUrl error: {}", .{err});
        return prepareThrowSymbol(vm, .UnknownError);
    };
    defer alloc.free(resp.body);
    if (resp.status != .ok) {
        log.debug("cacheUrl response status: {}", .{resp.status});
        return prepareThrowSymbol(vm, .UnknownError);
    } else {
        const entry = cache.saveNewSpecFile(alloc, specGroup, url, resp.body) catch cy.fatal();
        defer entry.deinit(alloc);
        const path = cache.allocSpecFilePath(alloc, entry) catch cy.fatal();
        defer alloc.free(path);
        return vm.retainOrAllocString(path) catch cy.fatal();
    }
}

pub fn execCmd(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (cy.isWasm) return vm.returnPanic("Unsupported.");
    const alloc = vm.allocator();
    const ivm = vm.internal();

    const obj = args[0].asHeapObject();
    var buf: std.ArrayListUnmanaged([]const u8) = .{};
    defer {
        for (buf.items) |arg| {
            alloc.free(arg);
        }
        buf.deinit(alloc);
    }
    for (obj.list.items()) |arg| {
        const str = try ivm.allocValueStr(arg);
        try buf.append(alloc, str);
    }

    const res = try std.ChildProcess.exec(.{
        .allocator = alloc,
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
    defer alloc.free(res.stdout);
    const out = try vm.retainOrAllocString(res.stdout);
    defer vm.release(out);
    try map.asHeapObject().map.set(ivm, outKey, out);
    // TODO: Use allocOwnedString
    defer alloc.free(res.stderr);
    const err = try vm.retainOrAllocString(res.stderr);
    defer vm.release(err);
    try map.asHeapObject().map.set(ivm, errKey, err);
    if (res.term == .Exited) {
        const exitedKey = try vm.retainOrAllocAstring("exited");
        defer vm.release(exitedKey);
        try map.asHeapObject().map.set(ivm, exitedKey, Value.initF64(@floatFromInt(res.term.Exited)));
    }
    return map;
}

pub fn exit(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
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

pub fn getInput(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const input = std.io.getStdIn().reader().readUntilDelimiterAlloc(vm.allocator(), '\n', 10e8) catch |err| {
        if (err == error.EndOfStream) {
            return prepareThrowSymbol(vm, .EndOfStream);
        } else cy.fatal();
    };
    defer vm.allocator().free(input);
    // TODO: Use allocOwnedString
    return stringOrThrow(vm, input);
}

pub fn getAllInput(vm: *cy.UserVM, _: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const input = try std.io.getStdIn().readToEndAlloc(vm.allocator(), 10e8);
    defer vm.allocator().free(input);
    // TODO: Use allocOwnString.
    return stringOrThrow(vm, input);
}

fn stringOrThrow(vm: *cy.UserVM, slice: []const u8) Value {
    if (cy.validateUtf8(slice)) |size| {
        if (size == slice.len) {
            return vm.retainOrAllocAstring(slice) catch cy.fatal();
        } else {
            return vm.retainOrAllocUstring(slice, @intCast(size)) catch cy.fatal();
        }
    } else {
        return prepareThrowSymbol(vm, .Unicode);
    }
}

pub fn readFile(vm: *cy.UserVM, args: [*]const Value, _: u8) anyerror!Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");

    const path = args[0].asString();
    const content = try std.fs.cwd().readFileAlloc(vm.allocator(), path, 10e8);
    defer vm.allocator().free(content);
    // TODO: Use allocOwnedString.
    return stringOrThrow(vm, content);
}

pub fn writeFile(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) anyerror!Value {
    if (!cy.hasStdFiles) return vm.returnPanic("Unsupported.");
    const path = args[0].asString();
    const content = try vm.getOrBufPrintValueRawStr(&cy.tempBuf, args[1]);
    try std.fs.cwd().writeFile(path, content);
    return Value.None;
}