const std = @import("std");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const fmt = @import("../fmt.zig");
const bindings = @import("bindings.zig");
const TagLit = bindings.TagLit;
const fromUnsupportedError = bindings.fromUnsupportedError;
const bt = cy.types.BuiltinTypeSymIds;
const ffi = @import("os_ffi.zig");

const log = stdx.log.scoped(.os);

pub var CFuncT: cy.TypeId = undefined;
pub var CStructT: cy.TypeId = undefined;

pub fn initModule(self: *cy.VMcompiler, mod: *cy.Module) linksection(cy.InitSection) !void {
    const vm = self.vm;

    const b = bindings.ModuleBuilder.init(self, mod);

    // Object Types.
    var id: u32 = undefined;
    var rSymId = try cy.sema.resolveObjectSym(self, mod.resolvedRootSymId, "CFunc", mod.id);
    var nameId = try cy.sema.ensureNameSym(self, "CFunc");
    CFuncT = try vm.addObjectTypeExt(mod.resolvedRootSymId, nameId, "CFunc", rSymId);
    vm.structs.buf[CFuncT].numFields = 3;
    id = try vm.ensureFieldSym("sym");
    try vm.addFieldSym(CFuncT, id, 0);
    id = try vm.ensureFieldSym("args");
    try vm.addFieldSym(CFuncT, id, 1);
    id = try vm.ensureFieldSym("ret");
    try vm.addFieldSym(CFuncT, id, 2);
    try mod.setObject(self, "CFunc", CFuncT, cy.NullId);

    rSymId = try cy.sema.resolveObjectSym(self, mod.resolvedRootSymId, "CStruct", mod.id);
    nameId = try cy.sema.ensureNameSym(self, "CStruct");
    CStructT = try vm.addObjectTypeExt(mod.resolvedRootSymId, nameId, "CStruct", rSymId);
    vm.structs.buf[CStructT].numFields = 2;
    id = try vm.ensureFieldSym("fields");
    try vm.addFieldSym(CStructT, id, 0);
    id = try vm.ensureFieldSym("type");
    try vm.addFieldSym(CStructT, id, 1);
    try mod.setObject(self, "CStruct", CStructT, cy.NullId);

    // Variables.
    try mod.setVar(self, "cpu", try self.buf.getOrPushStringValue(@tagName(builtin.cpu.arch)));
    if (builtin.cpu.arch.endian() == .Little) {
        try mod.setVar(self, "endian", cy.Value.initTagLiteral(@enumToInt(TagLit.little)));
    } else {
        try mod.setVar(self, "endian", cy.Value.initTagLiteral(@enumToInt(TagLit.big)));
    }
    if (cy.hasStdFiles) {
        const stdin = try cy.heap.allocFile(self.vm, std.io.getStdIn().handle);
        try mod.setVar(self, "stdin", stdin);
        const stdout = try cy.heap.allocFile(self.vm, std.io.getStdOut().handle);
        try mod.setVar(self, "stdout", stdout);
        const stderr = try cy.heap.allocFile(self.vm, std.io.getStdErr().handle);
        try mod.setVar(self, "stderr", stderr);
    } else {
        try mod.setVar(self, "stdin", Value.None);
        try mod.setVar(self, "stdout", Value.None);
        try mod.setVar(self, "stderr", Value.None);
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
    if (cy.isWasm) {
        try b.setFunc("access", &.{bt.Any, bt.TagLiteral}, bt.Any, bindings.nop1);
    } else {
        try b.setFunc("access", &.{bt.Any, bt.TagLiteral}, bt.Any, access);
    }
    try mod.setNativeTypedFunc(self, "args", &.{}, bt.List, osArgs);
    if (cy.isWasm) {
        try b.setFunc("bindLib", &.{bt.Any, bt.List}, bt.Any, bindings.nop2);
        try b.setFunc("bindLib", &.{bt.Any, bt.List, bt.Map}, bt.Any, bindings.nop3);
        try b.setFunc("copyFile", &.{bt.Any, bt.Any}, bt.Any, bindings.nop2);
        try b.setFunc("createDir", &.{bt.Any}, bt.Any, bindings.nop1);
        try b.setFunc("createFile", &.{bt.Any, bt.Boolean}, bt.Any, bindings.nop2);
        try b.setFunc("cwd", &.{}, bt.String, bindings.nop0);
        try b.setFunc("dirName", &.{ bt.Any }, bt.Any, bindings.nop1);
        try b.setFunc("exePath", &.{}, bt.String, bindings.nop0);
        try b.setFunc("free", &.{bt.Pointer}, bt.None, bindings.nop1);
        try b.setFunc("getEnv", &.{ bt.Any }, bt.Any, bindings.nop1);
        try b.setFunc("getEnvAll", &.{}, bt.Map, bindings.nop0);
        try b.setFunc("malloc", &.{bt.Number}, bt.Pointer, bindings.nop1);
    } else {
        try b.setFunc("bindLib", &.{bt.Any, bt.List}, bt.Any, bindLib);
        try b.setFunc("bindLib", &.{bt.Any, bt.List, bt.Map}, bt.Any, bindLibExt);
        try b.setFunc("copyFile", &.{bt.Any, bt.Any}, bt.Any, copyFile);
        try b.setFunc("createDir", &.{bt.Any}, bt.Any, createDir);
        try b.setFunc("createFile", &.{bt.Any, bt.Boolean}, bt.Any, createFile);
        try b.setFunc("cwd", &.{}, bt.String, cwd);
        try b.setFunc("dirName", &.{ bt.Any }, bt.Any, dirName);
        try b.setFunc("exePath", &.{}, bt.String, exePath);
        try b.setFunc("free", &.{bt.Pointer}, bt.None, free);
        if (builtin.os.tag == .windows) {
            try b.setFunc("getEnv", &.{ bt.Any }, bt.Any, bindings.nop1);
            try b.setFunc("getEnvAll", &.{}, bt.Map, bindings.nop0);
        } else {
            try b.setFunc("getEnv", &.{ bt.Any }, bt.Any, getEnv);
            try b.setFunc("getEnvAll", &.{}, bt.Map, getEnvAll);
        }
        try b.setFunc("malloc", &.{bt.Number}, bt.Pointer, malloc);
    }
    try b.setFunc("milliTime", &.{}, bt.Number, milliTime);
    if (cy.isWasm) {
        try b.setFunc("openDir", &.{bt.Any}, bt.Any, bindings.nop1);
        try b.setFunc("openDir", &.{bt.Any, bt.Boolean}, bt.Any, bindings.nop2);
        try b.setFunc("openFile", &.{bt.Any, bt.TagLiteral}, bt.Any, bindings.nop2);
        try b.setFunc("realPath", &.{bt.Any}, bt.Any, bindings.nop1);
        try b.setFunc("removeDir", &.{bt.Any}, bt.Any, bindings.nop1);
        try b.setFunc("removeFile", &.{bt.Any}, bt.Any, bindings.nop1);
        try b.setFunc("setEnv", &.{bt.Any, bt.Any}, bt.None, bindings.nop2);
    } else {
        try b.setFunc("openDir", &.{bt.Any}, bt.Any, openDir);
        try b.setFunc("openDir", &.{bt.Any, bt.Boolean}, bt.Any, openDir2);
        try b.setFunc("openFile", &.{bt.Any, bt.TagLiteral}, bt.Any, openFile);
        try b.setFunc("realPath", &.{bt.Any}, bt.Any, realPath);
        try b.setFunc("removeDir", &.{bt.Any}, bt.Any, removeDir);
        try b.setFunc("removeFile", &.{bt.Any}, bt.Any, removeFile);
        if (builtin.os.tag == .windows) {
            try b.setFunc("setEnv", &.{bt.Any, bt.Any}, bt.None, bindings.nop2);
        } else {
            try b.setFunc("setEnv", &.{bt.Any, bt.Any}, bt.None, setEnv);
        }
    }
    try b.setFunc("sleep", &.{bt.Number}, bt.None, sleep);
    if (cy.isWasm or builtin.os.tag == .windows) {
        try b.setFunc("unsetEnv", &.{bt.Any}, bt.None, bindings.nop1);
    } else {
        try b.setFunc("unsetEnv", &.{bt.Any}, bt.None, unsetEnv);
    }
}

pub fn deinitModule(c: *cy.VMcompiler, mod: cy.Module) !void {
    if (cy.hasStdFiles) {
        // Mark as closed to avoid closing.
        const stdin = (try mod.getVarVal(c, "stdin")).?;
        stdin.asHeapObject().file.closed = true;
        cy.arc.release(c.vm, stdin);

        const stdout = (try mod.getVarVal(c, "stdout")).?;
        stdout.asHeapObject().file.closed = true;
        cy.arc.release(c.vm, stdout);

        const stderr = (try mod.getVarVal(c, "stderr")).?;
        stderr.asHeapObject().file.closed = true;
        cy.arc.release(c.vm, stderr);
    }
}

fn openDir(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    return openDir2(vm, &[_]Value{ args[0], Value.False }, nargs);
}

fn openDir2(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const path = vm.valueToTempRawString(args[0]);
    const iterable = args[1].asBool();
    var fd: std.os.fd_t = undefined;
    if (iterable) {
        const dir = std.fs.cwd().openIterableDir(path, .{}) catch |err| {
            if (err == error.FileNotFound) {
                return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
            } else {
                return fromUnsupportedError("openDir", err, @errorReturnTrace());
            }
        };
        fd = dir.dir.fd;
    } else {
        const dir = std.fs.cwd().openDir(path, .{}) catch |err| {
            if (err == error.FileNotFound) {
                return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
            } else {
                return fromUnsupportedError("openDir", err, @errorReturnTrace());
            }
        };
        fd = dir.fd;
    }
    return vm.allocDir(fd, iterable) catch fatal();
}

fn removeDir(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const path = vm.valueToTempRawString(args[0]);
    std.fs.cwd().deleteDir(path) catch |err| {
        if (err == error.FileNotFound) {
            return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
        } else {
            return fromUnsupportedError("removeDir", err, @errorReturnTrace());
        }
    };
    return Value.True;
}

fn copyFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const src = vm.valueToTempRawString(args[0]);
    const alloc = vm.allocator();
    const srcDupe = alloc.dupe(u8, src) catch fatal();
    defer alloc.free(srcDupe);
    const dst = vm.valueToTempRawString(args[1]);
    std.fs.cwd().copyFile(srcDupe, std.fs.cwd(), dst, .{}) catch |err| {
        if (err == error.FileNotFound) {
            return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
        } else {
            return fromUnsupportedError("copyFile", err, @errorReturnTrace());
        }
    };
    return Value.True;
}

fn removeFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const path = vm.valueToTempRawString(args[0]);
    std.fs.cwd().deleteFile(path) catch |err| {
        if (err == error.FileNotFound) {
            return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
        } else {
            return fromUnsupportedError("removeFile", err, @errorReturnTrace());
        }
    };
    return Value.True;
}

fn createDir(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const path = vm.valueToTempRawString(args[0]);
    std.fs.cwd().makeDir(path) catch |err| {
        return fromUnsupportedError("createDir", err, @errorReturnTrace());
    };
    return Value.True;
}

fn createFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const path = vm.valueToTempRawString(args[0]);
    const truncate = args[1].asBool();
    const file = std.fs.cwd().createFile(path, .{ .truncate = truncate }) catch |err| {
        return fromUnsupportedError("createFile", err, @errorReturnTrace());
    };
    return vm.allocFile(file.handle) catch fatal();
}

pub fn access(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const path = vm.valueToTempRawString(args[0]);
    defer vm.release(args[0]);

    const mode = @intToEnum(TagLit, args[1].asTagLiteralId());
    const zmode: std.fs.File.OpenMode = switch (mode) {
        .read => .read_only,
        .write => .write_only,
        .readWrite => .read_write,
        else => {
            return Value.None;
        }
    };
    std.fs.cwd().access(path, .{ .mode = zmode }) catch |err| {
        if (err == error.FileNotFound) {
            return Value.initErrorTagLit(@enumToInt(TagLit.FileNotFound));
        } else if (err == error.PermissionDenied) {
            return Value.initErrorTagLit(@enumToInt(TagLit.PermissionDenied));
        } else {
            return fromUnsupportedError("access", err, @errorReturnTrace());
        }
    };
    return Value.True;
}

fn openFile(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const path = vm.valueToTempRawString(args[0]);
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
            return fromUnsupportedError("openFile", err, @errorReturnTrace());
        }
    };
    return vm.allocFile(file.handle) catch fatal();
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
    const key = vm.valueToTempRawString(args[0]);
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

pub fn free(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    const ptr = args[0].asHeapObject().pointer.ptr;
    std.c.free(ptr);
    return Value.None;
}

pub fn malloc(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const size = @floatToInt(usize, args[0].asF64());
    const ptr = std.c.malloc(size);
    return cy.heap.allocPointer(vm.internal(), ptr) catch stdx.fatal();
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
    const path = vm.valueToTempRawString(args[0]);
    defer vm.release(args[0]);
    const res = std.fs.cwd().realpathAlloc(vm.allocator(), path) catch |err| {
        return fromUnsupportedError("realPath", err, @errorReturnTrace());
    };
    defer vm.allocator().free(res);
    // TODO: Use allocOwnedString.
    return vm.allocStringInfer(res) catch stdx.fatal();
}

pub fn setEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const key = vm.valueToTempRawString(args[0]);
    const keyz = std.cstr.addNullByte(vm.allocator(), key) catch stdx.fatal();
    defer vm.allocator().free(keyz);

    const value = vm.valueToTempRawString(args[1]);
    const valuez = std.cstr.addNullByte(vm.allocator(), value) catch stdx.fatal();
    defer vm.allocator().free(valuez);
    _ = setenv(keyz, valuez, 1);
    return Value.None;
}
pub extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;

pub fn sleep(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer vm.release(args[0]);
    if (builtin.os.tag == .windows) {
        const ms = @floatToInt(u32, args[0].asF64());
        std.os.windows.kernel32.Sleep(ms);
    } else {
        const ms = args[0].asF64();
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
    const key = vm.valueToTempRawString(args[0]);
    const keyz = std.cstr.addNullByte(vm.allocator(), key) catch stdx.fatal();
    defer vm.allocator().free(keyz);
    _ = unsetenv(keyz);
    return Value.None;
}
pub extern "c" fn unsetenv(name: [*:0]const u8) c_int;

pub fn bindLib(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return @call(.never_inline, ffi.bindLib, .{vm, args, .{}}) catch |err| {
        if (err == error.InvalidArgument) {
            return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
        } else {
            return fromUnsupportedError("bindLib", err, @errorReturnTrace());
        }
    };
}

pub fn bindLibExt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    var configV = args[2];
    const ivm = vm.internal();
    const genMapV = vm.allocAstring("genMap") catch stdx.fatal();
    defer {
        vm.release(args[2]);
        vm.release(genMapV);
    }
    var config: ffi.BindLibConfig = .{};
    const val = ivm.getIndex(&configV, genMapV) catch stdx.fatal();
    if (val.isTrue()) {
        config.genMap = true;
    }
    return @call(.never_inline, ffi.bindLib, .{vm, args, config}) catch |err| {
        if (err == error.InvalidArgument) {
            return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
        } else {
            return fromUnsupportedError("bindLib", err, @errorReturnTrace());
        }
    };
}