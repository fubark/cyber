const std = @import("std");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const builtin = @import("builtin");
const cy = @import("../cyber.zig");
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const gvm = &vm_.gvm;
const TagLit = @import("bindings.zig").TagLit;

pub fn initModule(self: *cy.VMcompiler, alloc: std.mem.Allocator, spec: []const u8) !cy.Module {
    var mod = cy.Module{
        .syms = .{},
        .prefix = spec,
    };

    try mod.setVar(alloc, "cpu", try self.buf.getOrPushStringValue(@tagName(builtin.cpu.arch)));
    if (builtin.cpu.arch.endian() == .Little) {
        try mod.setVar(alloc, "endian", cy.Value.initTagLiteral(@enumToInt(TagLit.little)));
    } else {
        try mod.setVar(alloc, "endian", cy.Value.initTagLiteral(@enumToInt(TagLit.big)));
    }
    const stdin = try self.vm.allocFile(std.os.STDIN_FILENO);
    try mod.setVar(alloc, "stdin", stdin);
    try mod.setVar(alloc, "system", try self.buf.getOrPushStringValue(@tagName(builtin.os.tag)));

    try mod.setNativeFunc(alloc, "cwd", 0, cwd);
    try mod.setNativeFunc(alloc, "exePath", 0, exePath);
    try mod.setNativeFunc(alloc, "getEnv", 1, getEnv);
    try mod.setNativeFunc(alloc, "getEnvAll", 0, getEnvAll);
    try mod.setNativeFunc(alloc, "milliTime", 0, milliTime);
    try mod.setNativeFunc(alloc, "realPath", 1, realPath);
    try mod.setNativeFunc(alloc, "setEnv", 2, setEnv);
    try mod.setNativeFunc(alloc, "sleep", 1, sleep);
    try mod.setNativeFunc(alloc, "unsetEnv", 1, unsetEnv);
    return mod;
}

pub fn deinitModule(c: *cy.VMcompiler, mod: cy.Module) void {
    vm_.release(c.vm, mod.getVarVal("stdin").?);
}

pub fn cwd(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
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
        gvm.setIndex(map, key, val) catch stdx.fatal();
    }
    return map;
}

pub fn milliTime(_: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.initF64(@intToFloat(f64, std.time.milliTimestamp()));
}

pub fn realPath(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const path = vm.valueToTempString(args[0]);
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

pub fn sleep(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const ms = args[0].toF64();
    const secs = @floatToInt(u64, @divFloor(ms, 1000));
    const nsecs = @floatToInt(u64, 1e6 * (std.math.mod(f64, ms, 1000) catch stdx.fatal()));
    std.os.nanosleep(secs, nsecs);
    return Value.None;
}

pub fn unsetEnv(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const key = vm.valueToTempString(args[0]);
    const keyz = std.cstr.addNullByte(vm.allocator(), key) catch stdx.fatal();
    defer vm.allocator().free(keyz);
    _ = unsetenv(keyz);
    return Value.None;
}
pub extern "c" fn unsetenv(name: [*:0]const u8) c_int;