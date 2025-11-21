const std = @import("std");
const cy = @import("cyber.zig");
pub const wasm = @import("log_wasm.zig");
const builtin = @import("builtin");

const UseStd = !builtin.target.isWasm() or builtin.os.tag == .wasi;
const UseTimer = builtin.mode == .Debug and false;

var timer: ?std.time.Timer = null;

fn initTimerOnce() void {
    if (timer == null) {
        timer = std.time.Timer.start() catch unreachable;
    }
}

pub fn scoped(comptime Scope: @Type(.enum_literal)) type {
    return struct {
        pub fn trace(comptime format: []const u8, args: anytype) void {
            if (!cy.Trace) {
                return;
            }
            log(format, args);
        }

        pub fn tracev(comptime format: []const u8, args: anytype) void {
            if (!cy.Trace) {
                return;
            }
            if (!c.verbose()) {
                return;
            }
            log(format, args);
        }

        pub fn tracevIf(cond: bool, comptime format: []const u8, args: anytype) void {
            if (!cy.Trace) {
                return;
            }
            if (!c.verbose()) {
                return;
            }
            if (!cond) {
                return;
            }
            log(format, args);
        }

        pub fn log(comptime format: []const u8, args: anytype) void {
            if (UseTimer) {
                initTimerOnce();
                const elapsed = timer.?.read();
                const secs = elapsed / 1000000000;
                const msecs = (elapsed % 1000000000)/1000000;
                const prefix = @tagName(Scope) ++ ": {}.{}: ";
                cy.debug.log(prefix ++ format, .{secs, msecs} ++ args);
            } else {
                const prefix = @tagName(Scope) ++ ": ";
                cy.debug.log(prefix ++ format, args);
            }
        }
    };
}

const c = @import("capi.zig");

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
