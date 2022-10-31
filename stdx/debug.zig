const std = @import("std");
const stdx = @import("stdx.zig");
const builtin = @import("builtin");
pub const tracy = @import("tracy.zig");

const UseWasm = builtin.target.cpu.arch == .wasm32;
const log = stdx.log.scoped(.debug);

pub fn dassert(pred: bool) void {
    if (builtin.mode == .Debug) {
        std.debug.assert(pred);
    }
}

pub fn assertInRange(val: anytype, min: @TypeOf(val), max: @TypeOf(val)) void {
    if (val < min or val > max) {
        unreachable;
    }
}

pub fn panicFmt(comptime format: []const u8, args: anytype) noreturn {
    if (UseWasm) {
        log.err(format, args);
    }
    std.debug.panic(format, args);
}

pub fn panic(comptime msg: []const u8) noreturn {
    if (UseWasm) {
        // @panic can't print message in wasm so we use a logger that can.
        log.err(msg, .{});
    }
    @panic(msg);
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

const EnableTimerTrace = builtin.mode == .Debug;

const TimerTrace = struct {
    timer: if (EnableTimerTrace) stdx.time.Timer else void,

    pub fn end(self: *TimerTrace) void {
        if (EnableTimerTrace) {
            const now = self.timer.read();
            log.info("time: {d:.3}ms", .{ @intToFloat(f32, now) / 1e6 });
        }
    }

    pub fn endPrint(self: *TimerTrace, msg: []const u8) void {
        if (EnableTimerTrace) {
            const now = self.timer.read();
            log.info("{s}: {d:.3}ms", .{ msg, @intToFloat(f32, now) / 1e6 });
        }
    }
};

// Simple trace with std Timer.
pub fn trace() TimerTrace {
    if (EnableTimerTrace) {
        return .{
            .timer = stdx.time.Timer.start() catch unreachable,
        };
    } else {
        return .{
            .timer = {},
        };
    }
}
