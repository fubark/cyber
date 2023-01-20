const std = @import("std");
pub const wasm = @import("log_wasm.zig");
const builtin = @import("builtin");

const UseStd = !builtin.target.isWasm();
const UseTimer = builtin.mode == .Debug and true;

var timer: ?std.time.Timer = null;

fn initTimerOnce() void {
    if (timer == null) {
        timer = std.time.Timer.start() catch unreachable;
    }
}

pub fn scoped(comptime Scope: @Type(.EnumLiteral)) type {
    return struct {
        pub fn debug(comptime format: []const u8, args: anytype) void {
            if (UseStd) {
                if (UseTimer) {
                    initTimerOnce();
                    const elapsed = timer.?.read();
                    const secs = elapsed / 1000000000;
                    const msecs = (elapsed % 1000000000)/1000000;
                    std.log.scoped(Scope).debug("{}.{}: " ++ format, .{secs, msecs} ++ args);
                } else {
                    std.log.scoped(Scope).debug(format, args);
                }
            } else {
                wasm.scoped(Scope).debug(format, args);
            }
        }

        pub fn info(comptime format: []const u8, args: anytype) void {
            if (UseStd) {
                std.log.scoped(Scope).info(format, args);
            } else {
                wasm.scoped(Scope).info(format, args);
            }
        }

        pub fn warn(comptime format: []const u8, args: anytype) void {
            if (UseStd) {
                std.log.scoped(Scope).warn(format, args);
            } else {
                wasm.scoped(Scope).warn(format, args);
            }
        }

        pub fn err(comptime format: []const u8, args: anytype) void {
            if (UseStd) {
                std.log.scoped(Scope).err(format, args);
            } else {
                wasm.scoped(Scope).err(format, args);
            }
        }
    };
}

const default = if (UseStd) std.log.default else wasm.scoped(.default);

pub fn debug(comptime format: []const u8, args: anytype) void {
    default.info(format, args);
}

pub fn info(comptime format: []const u8, args: anytype) void {
    default.info(format, args);
}

pub fn err(comptime format: []const u8, args: anytype) void {
    default.err(format, args);
}

/// Used in C/C++ code to log synchronously.
const c_log = scoped(.c);
pub export fn zig_log(buf: [*c]const u8) void {
    c_log.debug("{s}", .{ buf });
}

pub export fn zig_log_u32(buf: [*c]const u8, val: u32) void {
    c_log.debug("{s}: {}", .{ buf, val });
}