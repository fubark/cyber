const std = @import("std");
const stdx = @import("stdx.zig");
const builtin = @import("builtin");
pub const tracy = @import("tracy.zig");

const UseWasm = builtin.target.cpu.arch == .wasm32;

pub fn assertInRange(val: anytype, min: @TypeOf(val), max: @TypeOf(val)) void {
    if (val < min or val > max) {
        unreachable;
    }
}

// Convenience function to shortcircuit in middle of code.
pub fn abort() void {
    unreachable;
}

// Useful for triggering user debug logic. Always false initially.
pub var flag: bool = false;

pub fn abortIfUserFlagSet() void {
    if (flag) {
        abort();
    }
}

pub fn compileErrorFmt(comptime format: []const u8, args: anytype) noreturn {
    const str = std.fmt.comptimePrint(format, args);
    @compileError(str);
}

