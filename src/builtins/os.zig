const std = @import("std");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const builtin = @import("builtin");
const tcc = @import("tcc");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const fmt = @import("../fmt.zig");
const bindings = @import("bindings.zig");
const TagLit = bindings.TagLit;

const log = stdx.log.scoped(.os);

const DumpCGen = builtin.mode == .Debug and false;

pub var CFuncT: cy.TypeId = undefined;
pub var CStructT: cy.TypeId = undefined;

pub fn initModule(self: *cy.VMcompiler, mod: *cy.Module) linksection(cy.InitSection) !void {
    const vm = self.vm;
    const wrapErrorFunc = bindings.wrapErrorFunc;

    // Object Types.
    var id: u32 = undefined;
    var nameId = try cy.sema.ensureNameSym(self, "CFunc");
    CFuncT = try vm.addObjectTypeExt(mod.resolvedRootSymId, nameId);
    vm.structs.buf[CFuncT].numFields = 3;
    id = try vm.ensureFieldSym("sym");
    try vm.addFieldSym(CFuncT, id, 0);
    id = try vm.ensureFieldSym("args");
    try vm.addFieldSym(CFuncT, id, 1);
    id = try vm.ensureFieldSym("ret");
    try vm.addFieldSym(CFuncT, id, 2);
    try mod.setObject(self, "CFunc", CFuncT);

    nameId = try cy.sema.ensureNameSym(self, "CStruct");
    CStructT = try vm.addObjectTypeExt(mod.resolvedRootSymId, nameId);
    vm.structs.buf[CStructT].numFields = 2;
    id = try vm.ensureFieldSym("fields");
    try vm.addFieldSym(CStructT, id, 0);
    id = try vm.ensureFieldSym("type");
    try vm.addFieldSym(CStructT, id, 1);
    try mod.setObject(self, "CStruct", CStructT);

    // Variables.
    try mod.setVar(self, "cpu", try self.buf.getOrPushStringValue(@tagName(builtin.cpu.arch)));
    if (builtin.cpu.arch.endian() == .Little) {
        try mod.setVar(self, "endian", cy.Value.initTagLiteral(@enumToInt(TagLit.little)));
    } else {
        try mod.setVar(self, "endian", cy.Value.initTagLiteral(@enumToInt(TagLit.big)));
    }
    if (cy.hasStdFiles) {
        if (builtin.os.tag != .windows) {
            const stdin = try cy.heap.allocFile(self.vm, std.os.STDIN_FILENO);
            try mod.setVar(self, "stdin", stdin);
        } else {
            // TODO: Use std.os.windows.STD_INPUT_HANDLE
            try mod.setVar(self, "stdin", Value.None);
        }
    } else {
        try mod.setVar(self, "stdin", Value.None);
    }
    if (builtin.cpu.arch.isWasm()) {
        try mod.setVar(self, "system", try self.buf.getOrPushStringValue("wasm"));
    } else {
        try mod.setVar(self, "system", try self.buf.getOrPushStringValue(@tagName(builtin.os.tag)));
    }
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        try mod.setVar(self, "vecBitSize", cy.Value.initF64(VecSize * 8));
    } else {
        try mod.setVar(self, "vecBitSize", cy.Value.initF64(0));
    }

    // Functions.
    try mod.setNativeFunc(self, "args", 0, osArgs);
    if (cy.isWasm) {
        try mod.setNativeFunc(self, "bindLib", 2, bindings.nop2);
        try mod.setNativeFunc(self, "bindLib", 3, bindings.nop3);
        try mod.setNativeFunc(self, "copyFile", 2, bindings.nop2);
        try mod.setNativeFunc(self, "createDir", 1, bindings.nop1);
        try mod.setNativeFunc(self, "createFile", 2, bindings.nop2);
        try mod.setNativeFunc(self, "cwd", 0, bindings.nop0);
        try mod.setNativeFunc(self, "dirName", 1, bindings.nop1);
        try mod.setNativeFunc(self, "exePath", 0, bindings.nop0);
        try mod.setNativeFunc(self, "free", 1, bindings.nop1);
        try mod.setNativeFunc(self, "getEnv", 1, bindings.nop1);
        try mod.setNativeFunc(self, "getEnvAll", 0, bindings.nop0);
        try mod.setNativeFunc(self, "malloc", 1, bindings.nop1);
    } else {
        try mod.setNativeFunc(self, "bindLib", 2, bindLib);
        try mod.setNativeFunc(self, "bindLib", 3, bindLibExt);
        try mod.setNativeFunc(self, "copyFile", 2, wrapErrorFunc("copyFile", copyFile));
        try mod.setNativeFunc(self, "createDir", 1, createDir);
        try mod.setNativeFunc(self, "createFile", 2, createFile);
        try mod.setNativeFunc(self, "cwd", 0, cwd);
        try mod.setNativeFunc(self, "dirName", 1, dirName);
        try mod.setNativeFunc(self, "exePath", 0, exePath);
        try mod.setNativeFunc(self, "free", 1, osFree);
        if (builtin.os.tag == .windows) {
            try mod.setNativeFunc(self, "getEnv", 1, bindings.nop1);
            try mod.setNativeFunc(self, "getEnvAll", 0, bindings.nop0);
        } else {
            try mod.setNativeFunc(self, "getEnv", 1, getEnv);
            try mod.setNativeFunc(self, "getEnvAll", 0, getEnvAll);
        }
        try mod.setNativeFunc(self, "malloc", 1, malloc);
    }
    try mod.setNativeFunc(self, "milliTime", 0, milliTime);
    if (cy.isWasm) {
        try mod.setNativeFunc(self, "openDir", 1, bindings.nop1);
        try mod.setNativeFunc(self, "openDir", 2, bindings.nop2);
        try mod.setNativeFunc(self, "openFile", 2, bindings.nop2);
        try mod.setNativeFunc(self, "removeDir", 1, bindings.nop1);
        try mod.setNativeFunc(self, "removeFile", 1, bindings.nop1);
        try mod.setNativeFunc(self, "realPath", 1, bindings.nop1);
        try mod.setNativeFunc(self, "setEnv", 2, bindings.nop2);
    } else {
        try mod.setNativeFunc(self, "openDir", 1, openDir);
        try mod.setNativeFunc(self, "openDir", 2, openDir2);
        try mod.setNativeFunc(self, "openFile", 2, openFile);
        try mod.setNativeFunc(self, "removeDir", 1, removeDir);
        try mod.setNativeFunc(self, "removeFile", 1, removeFile);
        try mod.setNativeFunc(self, "realPath", 1, realPath);
        if (builtin.os.tag == .windows) {
            try mod.setNativeFunc(self, "setEnv", 2, bindings.nop2);
        } else {
            try mod.setNativeFunc(self, "setEnv", 2, setEnv);
        }
    }
    try mod.setNativeFunc(self, "sleep", 1, sleep);
    if (cy.isWasm or builtin.os.tag == .windows) {
        try mod.setNativeFunc(self, "unsetEnv", 1, bindings.nop1);
    } else {
        try mod.setNativeFunc(self, "unsetEnv", 1, unsetEnv);
    }
}

pub fn deinitModule(c: *cy.VMcompiler, mod: cy.Module) !void {
    if (cy.hasStdFiles) {
        if (builtin.os.tag != .windows) {
            // Mark as closed to avoid closing.
            const stdin = (try mod.getVarVal(c, "stdin")).?;
            stdin.asHeapObject().file.closed = true;
            cy.arc.release(c.vm, stdin);
        }
    }
}

fn openDir(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    return openDir2(vm, &[_]Value{ args[0], Value.False }, nargs);
}

fn openDir2(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
    }
    const path = vm.valueToTempString(args[0]);
    const iterable = args[1].toBool();
    var fd: std.os.fd_t = undefined;
    if (iterable) {
        const dir = std.fs.cwd().openIterableDir(path, .{}) catch |err| {
            if (err == error.FileNotFound) {
                return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
            } else {
                fmt.printStderr("openDir {}", &.{fmt.v(err)});
                return Value.None;
            }
        };
        fd = dir.dir.fd;
    } else {
        const dir = std.fs.cwd().openDir(path, .{}) catch |err| {
            if (err == error.FileNotFound) {
                return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
            } else {
                fmt.printStderr("openDir {}", &.{fmt.v(err)});
                return Value.None;
            }
        };
        fd = dir.fd;
    }
    return vm.allocDir(fd, iterable) catch fatal();
}

fn removeDir(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
    }
    const path = vm.valueToTempString(args[0]);
    std.fs.cwd().deleteDir(path) catch |err| {
        fmt.printStderr("removeDir {}", &.{fmt.v(err)});
        return Value.None;
    };
    return Value.True;
}

fn copyFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) !Value {
    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const src = vm.valueToTempRawString(args[0]);
    const alloc = vm.allocator();
    const srcDupe = try alloc.dupe(u8, src);
    defer alloc.free(srcDupe);
    const dst = vm.valueToTempRawString(args[1]);
    try std.fs.cwd().copyFile(srcDupe, std.fs.cwd(), dst, .{});
    return Value.True;
}

fn removeFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
    }
    const path = vm.valueToTempString(args[0]);
    std.fs.cwd().deleteFile(path) catch |err| {
        fmt.printStderr("removeFile {}", &.{fmt.v(err)});
        return Value.None;
    };
    return Value.True;
}

fn createDir(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
    }
    const path = vm.valueToTempString(args[0]);
    std.fs.cwd().makeDir(path) catch |err| {
        fmt.printStderr("createDir {}", &.{fmt.v(err)});
        return Value.None;
    };
    return Value.True;
}

fn createFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
    }
    const path = vm.valueToTempString(args[0]);
    const truncate = args[1].toBool();
    const file = std.fs.cwd().createFile(path, .{ .truncate = truncate }) catch |err| {
        fmt.printStderr("createFile {}", &.{fmt.v(err)});
        return Value.None;
    };
    return vm.allocFile(file.handle) catch fatal();
}

fn openFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
    }
    const path = vm.valueToTempString(args[0]);
    if (args[1].isTagLiteral()) {
        const mode = @intToEnum(TagLit, args[1].asTagLiteralId());
        const zmode: std.fs.File.OpenMode = switch (mode) {
            .read => .read_only,
            .write => .write_only,
            .readWrite => .read_write,
            else => {
                return Value.None;
            }
        };
        const file = std.fs.cwd().openFile(path, .{ .mode = zmode }) catch |err| {
            if (err == error.FileNotFound) {
                return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
            } else {
                fmt.printStderr("openFile {}", &.{fmt.v(err)});
                return Value.None;
            }
        };
        return vm.allocFile(file.handle) catch fatal();
    } else {
        return Value.None;
    }
}

fn osArgs(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const alloc = vm.allocator();
    var iter = std.process.argsWithAllocator(alloc) catch stdx.fatal();
    defer iter.deinit();
    const listv = vm.allocEmptyList() catch stdx.fatal();
    const listo = listv.asHeapObject();
    while (iter.next()) |arg| {
        const argv = vm.allocRawString(arg) catch stdx.fatal();
        listo.list.append(vm.allocator(), argv);
    }
    return listv;
}

pub fn cwd(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const res = std.process.getCwdAlloc(vm.allocator()) catch fatal();
    defer vm.allocator().free(res);
    // TODO: Use allocOwnedString
    return vm.allocStringInfer(res) catch fatal();
}

pub fn exePath(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    const path = std.fs.selfExePathAlloc(vm.allocator()) catch fatal();
    defer vm.allocator().free(path);
    // TODO: Use allocOwnedString
    return vm.allocStringInfer(path) catch fatal();
}

pub fn getEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const key = vm.valueToTempString(args[0]);
    const res = std.os.getenv(key) orelse return Value.None;
    return vm.allocStringInfer(res) catch stdx.fatal();
}

pub fn getEnvAll(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    var env = std.process.getEnvMap(vm.allocator()) catch stdx.fatal();
    defer env.deinit();

    const map = vm.allocEmptyMap() catch stdx.fatal();
    var iter = env.iterator();
    while (iter.next()) |entry| {
        const key = vm.allocStringInfer(entry.key_ptr.*) catch stdx.fatal();
        const val = vm.allocStringInfer(entry.value_ptr.*) catch stdx.fatal();
        vm.internal().setIndex(map, key, val) catch stdx.fatal();
    }
    return map;
}

pub fn osFree(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    if (args[0].isObjectType(cy.OpaquePtrS)) {
        const ptr = args[0].asHeapObject().opaquePtr.ptr;
        std.c.free(ptr);
        return Value.None;
    } else {
        log.debug("Expected opaque ptr.", .{});
        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
    }
}

pub fn malloc(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const size = @floatToInt(usize, args[0].toF64());
    const ptr = std.c.malloc(size);
    return cy.heap.allocOpaquePtr(vm.internal(), ptr) catch stdx.fatal();
}

pub fn milliTime(_: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.initF64(@intToFloat(f64, stdx.time.getMilliTimestamp()));
}

pub fn dirName(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const path = vm.valueToTempRawString(args[0]);
    defer vm.release(args[0]);
    if (std.fs.path.dirname(path)) |res| {
        return vm.allocStringInfer(res) catch stdx.fatal();
    } else {
        return Value.None;
    }
}

pub fn realPath(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const path = vm.valueToTempString(args[0]);
    defer vm.release(args[0]);
    const res = std.fs.cwd().realpathAlloc(vm.allocator(), path) catch stdx.fatal();
    defer vm.allocator().free(res);
    // TODO: Use allocOwnedString.
    return vm.allocStringInfer(res) catch stdx.fatal();
}

pub fn setEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const key = vm.valueToString(args[0]) catch stdx.fatal();
    defer vm.allocator().free(key);
    const keyz = std.cstr.addNullByte(vm.allocator(), key) catch stdx.fatal();
    defer vm.allocator().free(keyz);

    const value = vm.valueToTempString(args[1]);
    const valuez = std.cstr.addNullByte(vm.allocator(), value) catch stdx.fatal();
    defer vm.allocator().free(valuez);
    _ = setenv(keyz, valuez, 1);
    return Value.None;
}
pub extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;

pub fn sleep(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    if (builtin.os.tag == .windows) {
        const ms = @floatToInt(u32, args[0].toF64());
        std.os.windows.kernel32.Sleep(ms);
    } else {
        const ms = args[0].toF64();
        const secs = @floatToInt(u64, @divFloor(ms, 1000));
        const nsecs = @floatToInt(u64, 1e6 * (std.math.mod(f64, ms, 1000) catch stdx.fatal()));
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
    const key = vm.valueToTempString(args[0]);
    const keyz = std.cstr.addNullByte(vm.allocator(), key) catch stdx.fatal();
    defer vm.allocator().free(keyz);
    _ = unsetenv(keyz);
    return Value.None;
}
pub extern "c" fn unsetenv(name: [*:0]const u8) c_int;

pub fn bindLib(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return @call(.never_inline, doBindLib, .{vm, args, .{}}) catch |err| {
        log.debug("{}", .{err});
        stdx.fatal();
    };
}

pub fn bindLibExt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    var configV = args[2];
    const ivm = @ptrCast(*cy.VM, vm);
    const genMapV = vm.allocAstring("genMap") catch stdx.fatal();
    defer {
        vm.release(args[2]);
        vm.release(genMapV);
    }
    var config: BindLibConfig = .{};
    const val = ivm.getIndex(&configV, genMapV) catch stdx.fatal();
    if (val.isTrue()) {
        config.genMap = true;
    }
    return @call(.never_inline, doBindLib, .{vm, args, config}) catch |err| {
        log.debug("{}", .{err});
        stdx.fatal();
    };
}

const BindLibConfig = struct {
    /// Whether bindLib generates the binding to an anonymous object type as methods
    /// or a map with functions.
    genMap: bool = false,
};

fn dlopen(path: []const u8) !std.DynLib {
    if (builtin.os.tag == .linux and builtin.link_libc) {
        const path_c = try std.os.toPosixPath(path);
        // Place the lookup scope of the symbols in this library ahead of the global scope.
        const RTLD_DEEPBIND = 0x00008;
        return std.DynLib{
            .handle = std.os.system.dlopen(&path_c, std.os.system.RTLD.LAZY | RTLD_DEEPBIND) orelse {
                return error.FileNotFound;
            }
        };
    } else {
        return std.DynLib.open(path);
    }
}

fn doBindLib(vm: *cy.UserVM, args: [*]const Value, config: BindLibConfig) !Value {
    const Context = struct {
        ivm: *cy.VM,
        alloc: std.mem.Allocator,
        symToCStructFields: std.AutoHashMapUnmanaged(u32, *cy.CyList) = .{},

        fn deinit(self: *@This()) void {
            self.symToCStructFields.deinit(self.alloc);
        }

        fn genFuncReleaseOps(self: *@This(), w: anytype, cargs: []const Value) !void {
            for (cargs, 0..) |carg, i| {
                if (carg.isObjectType(cy.SymbolT)) {
                    try w.print("  icyRelease(vm, args[{}]);\n", .{i});
                    // Free any child temps.
                    const objType = carg.asPointer(*cy.heap.Symbol);
                    const fields = self.symToCStructFields.get(objType.symId).?.items();
                    for (fields, 0..) |field, fidx| {
                        if (!field.isObjectType(cy.SymbolT)) {
                            const fieldType = field.asTagLiteralId();
                            switch (@intToEnum(TagLit, fieldType)) {
                                .charPtrZ => {
                                    try w.print("  icyFree(s{}.f{});\n", .{i, fidx});
                                },
                                else => {},
                            } 
                        }
                    }
                } else {
                    const argTag = carg.asTagLiteralId();
                    switch (@intToEnum(TagLit, argTag)) {
                        .charPtrZ => {
                            try w.print("  icyRelease(vm, args[{}]);\n", .{i});
                            try w.print("  icyFree(str{});\n", .{i});
                        },
                        .dupeCharPtrZ => {
                            try w.print("  icyRelease(vm, args[{}]);\n", .{i});
                        },
                        .voidPtr => {
                            try w.print("  icyRelease(vm, args[{}]);\n", .{i});
                        },
                        else => {},
                    }
                }
            }
        }
    };
    const CFuncData = struct {
        decl: Value,
        symPtr: *anyopaque,
    };
    const S = struct {
        fn toCType(ivm: *cy.VM, val: Value) []const u8 {
            const tag = val.asTagLiteralId();
            switch (@intToEnum(TagLit, tag)) {
                .bool => return "bool",
                .char => return "int8_t",
                .uchar => return "uint8_t",
                .short => return "int16_t",
                .ushort => return "uint16_t",
                .int => return "int",
                .uint => return "uint32_t",
                .long => return "int64_t",
                .ulong => return "uint64_t",
                .usize => return "size_t",
                .float => return "float",
                .double => return "double",
                .charPtrZ => return "char*",
                .dupeCharPtrZ => return "char*",
                .voidPtr => return "void*",
                .void => return "void",
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getTagLitName(tag) }),
            }
        }

        /// Assumes `uint64_t* args` is available in the scope.
        fn printToCValueFromArg(ivm: *cy.VM, w: anytype, argType: Value, i: usize) !void {
            const tag = argType.asTagLiteralId();
            switch (@intToEnum(TagLit, tag)) {
                .bool => {
                    try w.print("(args[{}] == 0x7FFC000100000001)?1:0", .{i});
                },
                .char => {
                    try w.print("(int8_t)*(double*)&args[{}]", .{i});
                },
                .uchar => {
                    try w.print("(uint8_t)*(double*)&args[{}]", .{i});
                },
                .short => {
                    try w.print("(int16_t)*(double*)&args[{}]", .{i});
                },
                .ushort => {
                    try w.print("(uint16_t)*(double*)&args[{}]", .{i});
                },
                .int => {
                    try w.print("(int)*(double*)&args[{}]", .{i});
                },
                .uint => {
                    try w.print("(uint32_t)*(double*)&args[{}]", .{i});
                },
                .long => {
                    try w.print("(int64_t)*(double*)&args[{}]", .{i});
                },
                .ulong => {
                    try w.print("(uint64_t)*(double*)&args[{}]", .{i});
                },
                .usize => {
                    try w.print("(size_t)*(double*)&args[{}]", .{i});
                },
                .float => {
                    try w.print("(float)*(double*)&args[{}]", .{i});
                },
                .double => {
                    try w.print("*(double*)&args[{}]", .{i});
                },
                .dupeCharPtrZ => {
                    try w.print("icyToCStr(vm, args[{}])", .{i});
                },
                .charPtrZ => {
                    try w.print("str{}", .{i});
                },
                .voidPtr => {
                    try w.print("icyGetPtr(args[{}])", .{i});
                },
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getTagLitName(tag) }),
            }
        }

        /// If the c value is converted to a number, this assumes `cval` is already a double.
        fn printCyValue(ivm: *cy.VM, w: anytype, argType: Value, cval: []const u8) !void {
            const tag = argType.asTagLiteralId();
            switch (@intToEnum(TagLit, tag)) {
                .char,
                .uchar,
                .short,
                .ushort,
                .int,
                .uint,
                .long,
                .ulong,
                .usize,
                .float,
                .double => {
                    // Assumes cval is already converted to double.
                    try w.print("*(uint64_t*)&{s}", .{cval});
                },
                .charPtrZ => {
                    try w.print("icyFromCStr(vm, {s})", .{cval});
                },
                .voidPtr => {
                    try w.print("icyAllocOpaquePtr(vm, {s})", .{cval});
                },
                .void => {
                    try w.print("0x7FFC000000000000", .{});
                },
                .bool => {
                    try w.print("({s} == 1) ? 0x7FFC000100000001 : 0x7FFC000100000000", .{cval});
                },
                else => stdx.panicFmt("Unsupported arg type: {s}", .{ ivm.getTagLitName(tag) }),
            }
        }
    };

    const path = args[0];
    const alloc = vm.allocator();
    const ivm = @ptrCast(*cy.VM, vm);

    var success = false;

    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }

    var lib = try alloc.create(std.DynLib);
    defer {
        if (!success) {
            alloc.destroy(lib);
        }
    }

    if (path.isNone()) {
        if (builtin.os.tag == .macos) {
            const exe = try std.fs.selfExePathAlloc(alloc);
            defer alloc.free(exe);
            lib.* = try dlopen(exe);
        } else {
            lib.* = try dlopen("");
        }
    } else {
        lib.* = dlopen(vm.valueToTempString(path)) catch |err| {
            if (err == error.FileNotFound) {
                return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
            } else {
                return err;
            }
        };
    }

    var ctx = Context{
        .ivm = ivm,
        .alloc = alloc,
        .symToCStructFields = .{},
    };
    defer ctx.deinit();

    var cfuncs: std.ArrayListUnmanaged(CFuncData) = .{};
    defer cfuncs.deinit(alloc);

    const decls = args[1].asPointer(*cy.CyList);

    // Check that symbols exist and build the model.
    const symF = try ivm.ensureFieldSym("sym");
    const fieldsF = try ivm.ensureFieldSym("fields");
    const typeF = try ivm.ensureFieldSym("type");
    for (decls.items()) |decl| {
        if (decl.isObjectType(CFuncT)) {
            const sym = vm.valueToTempString(try ivm.getField(decl, symF));
            const symz = try std.cstr.addNullByte(alloc, sym);
            defer alloc.free(symz);
            if (lib.lookup(*anyopaque, symz)) |ptr| {
                try cfuncs.append(alloc, .{
                    .decl = decl,
                    .symPtr = ptr,
                });
            } else {
                log.debug("Missing sym: '{s}'", .{sym});
                return Value.initErrorTagLit(@enumToInt(TagLit.MissingSymbol));
            }
        } else if (decl.isObjectType(CStructT)) {
            const val = try ivm.getField(decl, typeF);
            if (val.isObjectType(cy.SymbolT)) {
                const objType = val.asPointer(*cy.heap.Symbol);
                if (objType.symType == @enumToInt(cy.heap.SymbolType.object)) {
                    if (!ctx.symToCStructFields.contains(objType.symId)) {
                        const fields = try ivm.getField(decl, fieldsF);
                        try ctx.symToCStructFields.put(alloc, objType.symId, fields.asPointer(*cy.CyList));
                    } else {
                        log.debug("Object type already declared.", .{});
                        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
                    }
                } else {
                    log.debug("Not an object Symbol", .{});
                    return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
                }
            } else {
                log.debug("Not a Symbol", .{});
                return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
            }
        } else {
            log.debug("Not a CFunc or CStruct", .{});
            return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
        }
    }

    // Generate c code.
    var csrc: std.ArrayListUnmanaged(u8) = .{};
    defer csrc.deinit(alloc);
    const w = csrc.writer(alloc);

    try w.print(
        \\#define bool _Bool
        \\#define int64_t long long
        \\#define uint64_t unsigned long long
        // TODO: Check for 32bit addressing.
        \\#define size_t uint64_t
        \\#define int8_t signed char
        \\#define uint8_t unsigned char
        \\#define int16_t short
        \\#define uint16_t unsigned short
        \\#define uint32_t unsigned int
        \\#define PointerMask 0xFFFC000000000000
        \\typedef struct UserVM *UserVM;
        \\extern char* icyToCStr(UserVM*, uint64_t);
        \\extern uint64_t icyFromCStr(UserVM*, char*);
        \\extern void icyFree(void*);
        \\extern void icyRelease(UserVM*, uint64_t);
        \\extern void* icyGetPtr(uint64_t);
        \\extern uint64_t icyAllocObject(UserVM*, uint32_t);
        \\extern uint64_t icyAllocOpaquePtr(UserVM*, void*);
        // \\extern int printf(char* fmt, ...);
        \\
    , .{});

    var iter = ctx.symToCStructFields.iterator();
    while (iter.next()) |e| {
        const objSymId = e.key_ptr.*;
        const fields = e.value_ptr.*;

        // Generate C struct.
        try w.print("typedef struct Struct{} {{\n", .{objSymId});
        for (fields.items(), 0..) |field, i| {
            try w.print("  {s} f{};\n", .{S.toCType(ivm, field), i});
        }
        try w.print("}} Struct{};\n", .{objSymId});

        // Generate to C struct conv.
        try w.print("Struct{} toStruct{}(UserVM* vm, uint64_t val) {{\n", .{objSymId, objSymId});
        // Get pointer to first cyber object field.
        try w.print("  uint64_t* args = (uint64_t*)(val & ~PointerMask) + 1;\n", .{});
        try w.print("  Struct{} res;\n", .{objSymId, });
        for (fields.items(), 0..) |field, i| {
            try w.print("  res.f{} = ", .{i});
            if (field.isObjectType(cy.SymbolT)) {
                const objType = field.asPointer(*cy.heap.Symbol);
                try w.print("toStruct{}(vm, args[{}])", .{objType.symId, i});
            } else {
                const tag = field.asTagLiteralId();
                switch (@intToEnum(TagLit, tag)) {
                    .charPtrZ => {
                        try w.print("icyToCStr(vm, args[{}])", .{i});
                    },
                    else => {
                        try S.printToCValueFromArg(ivm, w, field, i);
                    }
                }
            }
            try w.print(";\n", .{});
        }
        try w.print("  return res;\n", .{});
        try w.print("}}\n", .{});

        // Generate from C struct conv.
        try w.print("uint64_t fromStruct{}(UserVM* vm, Struct{} val) {{\n", .{objSymId, objSymId});
        try w.print("  uint64_t obj = icyAllocObject(vm, {});\n", .{objSymId});
        try w.print("  uint64_t* args = (uint64_t*)(obj & ~PointerMask) + 1;\n", .{});
        var buf: [8]u8 = undefined;
        for (fields.items(), 0..) |field, i| {
            if (!field.isObjectType(cy.SymbolT)) {
                const fieldTag = field.asTagLiteralId();
                switch (@intToEnum(TagLit, fieldTag)) {
                    .char,
                    .uchar,
                    .short,
                    .ushort,
                    .int,
                    .uint,
                    .long,
                    .ulong,
                    .usize,
                    .float => {
                        try w.print("  double arg{} = (double)val.f{};\n", .{i, i});
                        const argStr = try std.fmt.bufPrint(&buf, "arg{}", .{i});
                        try w.print("  args[{}] = ", .{i});
                        try S.printCyValue(ivm, w, field, argStr);
                        try w.print(";\n", .{});
                        continue;
                    },
                    else => {},
                }
            }
            const argStr = try std.fmt.bufPrint(&buf, "val.f{}", .{i});
            try w.print("  args[{}] = ", .{i});
            try S.printCyValue(ivm, w, field, argStr);
            try w.print(";\n", .{});
        }
        try w.print("  return obj;\n", .{});
        try w.print("}}\n", .{});

        // Generate ptrTo[Object].
        if (config.genMap) {
            try w.print("uint64_t cyPtrTo{s}(UserVM* vm, uint64_t* args, char numArgs) {{\n", .{ivm.structs.buf[objSymId].name});
        } else {
            try w.print("uint64_t cyPtrTo{s}(UserVM* vm, uint64_t recv, uint64_t* args, char numArgs) {{\n", .{ivm.structs.buf[objSymId].name});
        }
        try w.print("  uint64_t ptr = *((uint64_t*)(args[0] & ~PointerMask) + 1);\n", .{});
        try w.print("  uint64_t res = fromStruct{}(vm, *(Struct{}*)ptr);\n", .{objSymId, objSymId});
        try w.print("  icyRelease(vm, args[0]);\n", .{});
        if (!config.genMap) {
            try w.print("  icyRelease(vm, recv);\n", .{});
        }
        try w.print("  return res;\n", .{});
        try w.print("}}\n", .{});
    }

    // Begin func binding generation.

    const argsf = try ivm.ensureFieldSym("args");
    const retf = try ivm.ensureFieldSym("ret");
    for (cfuncs.items) |cfunc| {
        const sym = vm.valueToTempString(try ivm.getField(cfunc.decl, symF));
        const cargsv = try ivm.getField(cfunc.decl, argsf);
        const ret = try ivm.getField(cfunc.decl, retf);

        const cargs = cargsv.asPointer(*cy.CyList).items();

        // Emit extern declaration.
        if (ret.isObjectType(cy.SymbolT)) {
            const objType = ret.asPointer(*cy.heap.Symbol);
            if (ctx.symToCStructFields.contains(objType.symId)) {
                try w.print("extern Struct{} {s}(", .{objType.symId, sym});
            } else {
                log.debug("CStruct not declared.", .{});
                return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
            }
        } else {
            try w.print("extern {s} {s}(", .{S.toCType(ivm, ret), sym});
        }
        if (cargs.len > 0) {
            const lastArg = cargs.len-1;
            for (cargs, 0..) |carg, i| {
                if (carg.isObjectType(cy.SymbolT)) {
                    const objType = carg.asPointer(*cy.heap.Symbol);
                    if (ctx.symToCStructFields.contains(objType.symId)) {
                        try w.print("Struct{}", .{objType.symId});
                    } else {
                        log.debug("CStruct not declared.", .{});
                        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
                    }
                } else {
                    try w.print("{s}", .{S.toCType(ivm, carg)});
                }
                if (i != lastArg) {
                    try w.print(", ", .{});
                }
            }
        }
        try w.print(");\n", .{});

        if (config.genMap) {
            try w.print("uint64_t cy{s}(UserVM* vm, uint64_t* args, char numArgs) {{\n", .{sym});
        } else {
            try w.print("uint64_t cy{s}(UserVM* vm, uint64_t recv, uint64_t* args, char numArgs) {{\n", .{sym});
        }
        // w.print("  printF64(*(double*)&args[0]);\n", .{}) catch stdx.fatal();

        // Gen temp args.
        if (cargs.len > 0) {
            for (cargs, 0..) |carg, i| {
                if (carg.isObjectType(cy.SymbolT)) {
                    const objType = carg.asPointer(*cy.heap.Symbol);
                    try w.print("Struct{} s{} = toStruct{}(vm, args[{}]);\n", .{objType.symId, i, objType.symId, i});
                } else {
                    const tag = carg.asTagLiteralId();
                    switch (@intToEnum(TagLit, tag)) {
                        .charPtrZ => {
                            try w.print("  char* str{} = icyToCStr(vm, args[{}]);\n", .{i, i});
                        },
                        else => {},
                    }
                }
            }
        }

        // Gen call.
        if (ret.isObjectType(cy.SymbolT)) {
            const objType = ret.asPointer(*cy.heap.Symbol);
            if (ctx.symToCStructFields.contains(objType.symId)) {
                try w.print("  Struct{} res = {s}(", .{objType.symId, sym});
            } else {
                log.debug("CStruct not declared.", .{});
                return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
            }
        } else {
            const retTag = ret.asTagLiteralId();
            switch (@intToEnum(TagLit, retTag)) {
                .char,
                .uchar,
                .short,
                .ushort,
                .int,
                .uint,
                .long,
                .ulong,
                .usize,
                .float => {
                    try w.print("  double res = (double){s}(", .{sym});
                },
                .double => {
                    try w.print("  double res = {s}(", .{sym});
                },
                .charPtrZ => {
                    try w.print("  char* res = {s}(", .{sym});
                },
                .voidPtr => {
                    try w.print("  void* res = {s}(", .{sym});
                },
                .void => {
                    try w.print("  {s}(", .{sym});
                },
                .bool => {
                    try w.print("  bool res = {s}(", .{sym});
                },
                else => stdx.panicFmt("Unsupported return type: {s}", .{ ivm.getTagLitName(retTag) }),
            }
        }

        // Gen args.
        if (cargs.len > 0) {
            const lastArg = cargs.len-1;
            for (cargs, 0..) |carg, i| {
                if (carg.isObjectType(cy.SymbolT)) {
                    try w.print("s{}", .{i});
                } else {
                    try S.printToCValueFromArg(ivm, w, carg, i);
                }
                if (i != lastArg) {
                    try w.print(", ", .{});
                }
            }
        }

        // End of args.
        try w.print(");\n", .{});

        try ctx.genFuncReleaseOps(w, cargs);
        if (!config.genMap) {
            // Release obj.
            try w.print("  icyRelease(vm, recv);\n", .{});
        }

        // Gen return.
        try w.print("  return ", .{});
        if (ret.isObjectType(cy.SymbolT)) {
            const objType = ret.asPointer(*cy.heap.Symbol);
            try w.print("fromStruct{}(vm, res)", .{ objType.symId });
        } else {
            try S.printCyValue(ivm, w, ret, "res");
        }
        try w.print(";\n", .{});

        try w.print("}}\n", .{});
    }

    try w.writeByte(0);
    if (DumpCGen) {
        log.debug("{s}", .{csrc.items});
    }

    const state = tcc.tcc_new();
    // Don't include libtcc1.a.
    tcc.tcc_set_options(state, "-nostdlib");
    _ = tcc.tcc_set_output_type(state, tcc.TCC_OUTPUT_MEMORY);

    if (tcc.tcc_compile_string(state, csrc.items.ptr) == -1) {
        stdx.panic("Failed to compile c source.");
    }

    // const __floatundisf = @extern(*anyopaque, .{ .name = "__floatundisf", .linkage = .Strong });
    if (builtin.cpu.arch != .aarch64) {
        _ = tcc.tcc_add_symbol(state, "__fixunsdfdi", __fixunsdfdi);
        _ = tcc.tcc_add_symbol(state, "__floatundidf", __floatundidf);
    }
    // _ = tcc.tcc_add_symbol(state, "__floatundisf", __floatundisf);
    // _ = tcc.tcc_add_symbol(state, "printf", std.c.printf);
    // _ = tcc.tcc_add_symbol(state, "breakpoint", breakpoint);
    _ = tcc.tcc_add_symbol(state, "icyFromCStr", fromCStr);
    _ = tcc.tcc_add_symbol(state, "icyToCStr", toCStr);
    _ = tcc.tcc_add_symbol(state, "icyFree", free);
    _ = tcc.tcc_add_symbol(state, "icyRelease", cRelease);
    _ = tcc.tcc_add_symbol(state, "icyGetPtr", cGetPtr);
    _ = tcc.tcc_add_symbol(state, "icyAllocOpaquePtr", cAllocOpaquePtr);
    _ = tcc.tcc_add_symbol(state, "icyAllocObject", cAllocObject);
    if (builtin.cpu.arch == .aarch64) {
        _ = tcc.tcc_add_symbol(state, "memmove", memmove);
    }

    // Add binded symbols.
    for (cfuncs.items) |cfunc| {
        const sym = vm.valueToTempString(try ivm.getField(cfunc.decl, symF));
        const symz = try std.cstr.addNullByte(alloc, sym);
        defer alloc.free(symz);
        _ = tcc.tcc_add_symbol(state, symz.ptr, cfunc.symPtr);
    }

    if (tcc.tcc_relocate(state, tcc.TCC_RELOCATE_AUTO) < 0) {
        stdx.panic("Failed to relocate compiled code.");
    }

    if (config.genMap) {
        // Create map with binded C-functions as functions.
        const map = vm.allocEmptyMap() catch stdx.fatal();

        const cyState = try cy.heap.allocTccState(ivm, state.?, lib);
        cy.arc.retainInc(ivm, cyState, @intCast(u32, cfuncs.items.len + ctx.symToCStructFields.size - 1));

        for (cfuncs.items) |cfunc| {
            const sym = vm.valueToTempString(try ivm.getField2(cfunc.decl, symF));
            const symGen = try std.fmt.allocPrint(alloc, "cy{s}{u}", .{sym, 0});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                stdx.panic("Failed to get symbol.");
            };

            const symKey = vm.allocAstring(sym) catch stdx.fatal();
            const cargsv = try ivm.getField2(cfunc.decl, argsf);
            const cargs = cargsv.asPointer(*cy.CyList).items();
            const func = stdx.ptrAlignCast(*const fn (*cy.UserVM, [*]const Value, u8) Value, funcPtr);

            const rFuncSigId = try vm.ensureUntypedFuncSig(@intCast(u32, cargs.len));
            const funcVal = cy.heap.allocNativeFunc1(ivm, func, @intCast(u32, cargs.len), rFuncSigId, cyState) catch stdx.fatal();
            ivm.setIndex(map, symKey, funcVal) catch stdx.fatal();
        }
        iter = ctx.symToCStructFields.iterator();
        while (iter.next()) |e| {
            const objSymId = e.key_ptr.*;
            const typeName = ivm.structs.buf[objSymId].name;
            const symGen = try std.fmt.allocPrint(alloc, "cyPtrTo{s}{u}", .{typeName, 0});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                stdx.panic("Failed to get symbol.");
            };

            const symKey = vm.allocAstringConcat("ptrTo", typeName) catch stdx.fatal();
            const func = stdx.ptrAlignCast(*const fn (*cy.UserVM, [*]const Value, u8) Value, funcPtr);

            const rFuncSigId = try vm.ensureUntypedFuncSig(1);
            const funcVal = cy.heap.allocNativeFunc1(ivm, func, 1, rFuncSigId, cyState) catch stdx.fatal();
            ivm.setIndex(map, symKey, funcVal) catch stdx.fatal();
        }
        success = true;
        return map;
    } else {
        // Create anonymous struct with binded C-functions as methods.
        const sid = try ivm.addAnonymousStruct("BindLib");
        const tccField = try ivm.ensureFieldSym("tcc");
        ivm.structs.buf[sid].numFields = 1;
        try ivm.addFieldSym(sid, tccField, 0);

        const cyState = try cy.heap.allocTccState(ivm, state.?, lib);
        for (cfuncs.items) |cfunc| {
            const sym = vm.valueToTempString(try ivm.getField2(cfunc.decl, symF));
            const cySym = try std.fmt.allocPrint(alloc, "cy{s}{u}", .{sym, 0});
            defer alloc.free(cySym);
            const funcPtr = tcc.tcc_get_symbol(state, cySym.ptr) orelse {
                stdx.panic("Failed to get symbol.");
            };

            const cargsv = try ivm.getField2(cfunc.decl, argsf);
            const cargs = cargsv.asPointer(*cy.CyList).items();

            const func = stdx.ptrAlignCast(cy.NativeObjFuncPtr, funcPtr);

            const methodSym = try ivm.ensureMethodSymKey(sym, @intCast(u32, cargs.len));
            try @call(.never_inline, ivm.addMethodSym, .{sid, methodSym, cy.MethodSym.initNativeFunc1(func) });
        }
        iter = ctx.symToCStructFields.iterator();
        while (iter.next()) |e| {
            const objSymId = e.key_ptr.*;
            const typeName = ivm.structs.buf[objSymId].name;
            const symGen = try std.fmt.allocPrint(alloc, "cyPtrTo{s}{u}", .{typeName, 0});
            defer alloc.free(symGen);
            const funcPtr = tcc.tcc_get_symbol(state, symGen.ptr) orelse {
                stdx.panic("Failed to get symbol.");
            };
            const func = stdx.ptrAlignCast(cy.NativeObjFuncPtr, funcPtr);

            const methodName = try std.fmt.allocPrint(alloc, "ptrTo{s}", .{typeName});
            defer alloc.free(methodName);
            const methodSym = try ivm.ensureMethodSymKey2(methodName, 1, true);
            try @call(.never_inline, ivm.addMethodSym, .{sid, methodSym, cy.MethodSym.initNativeFunc1(func) });
        }
        success = true;
        return try vm.allocObjectSmall(sid, &.{cyState});
    }
}

extern fn __floatundidf(u64) f64;
extern fn __fixunsdfdi(f64) u64;
extern fn memmove(dst: *anyopaque, src: *anyopaque, num: usize) *anyopaque;

fn fromCStr(vm: *cy.UserVM, ptr: [*:0]const u8) callconv(.C) Value {
    const slice = std.mem.span(ptr);
    return vm.allocRawString(slice) catch stdx.fatal();
}

fn toCStr(vm: *cy.UserVM, val: Value) callconv(.C) [*]const u8 {
    var str: []const u8 = undefined;
    if (val.isRawString()) {
        str = val.asRawString();
    } else {
        str = vm.valueToTempString(val);
    }
    const dupe = @ptrCast([*]u8, std.c.malloc(str.len + 1));
    std.mem.copy(u8, dupe[0..str.len], str);
    dupe[str.len] = 0;
    return dupe;
}

fn free(ptr: ?*anyopaque) callconv(.C) void {
    std.c.free(ptr);
}

fn cRelease(vm: *cy.UserVM, val: Value) callconv(.C) void {
    vm.release(val);
}

fn cGetPtr(val: Value) callconv(.C) ?*anyopaque {
    return val.asPointer(*cy.OpaquePtr).ptr;
}

fn cAllocOpaquePtr(vm: *cy.UserVM, ptr: ?*anyopaque) callconv(.C) Value {
    return cy.heap.allocOpaquePtr(vm.internal(), ptr) catch stdx.fatal();
}

fn cAllocObject(vm: *cy.UserVM, id: u32) callconv(.C) Value {
    const ivm = vm.internal();
    if (ivm.structs.buf[id].numFields <= 4) {
        return cy.heap.allocEmptyObjectSmall(ivm, id) catch stdx.fatal();
    } else {
        return cy.heap.allocEmptyObject(ivm, id, ivm.structs.buf[id].numFields) catch stdx.fatal();
    }
}

fn breakpoint() callconv(.C) void {
    @breakpoint();
}
