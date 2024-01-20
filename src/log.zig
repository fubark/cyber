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

fn printStderr(comptime format: []const u8, args: anytype) void {
    const stderr = std.io.getStdErr().writer();
    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    stderr.print(format, args) catch @panic("print error");
}

pub fn scoped(comptime Scope: @Type(.EnumLiteral)) type {
    return struct {
        pub fn trace(comptime format: []const u8, args: anytype) void {
            if (!cy.Trace) {
                return;
            }
            trace_(format, args);
        }

        pub fn tracev(comptime format: []const u8, args: anytype) void {
            if (!cy.Trace) {
                return;
            }
            if (!cy.verbose) {
                return;
            }
            trace_(format, args);
        }

        inline fn trace_(comptime format: []const u8, args: anytype) void {
            if (UseTimer) {
                initTimerOnce();
                const elapsed = timer.?.read();
                const secs = elapsed / 1000000000;
                const msecs = (elapsed % 1000000000)/1000000;
                const prefix = @tagName(Scope) ++ ": {}.{}: ";
                zfmt(prefix ++ format, .{secs, msecs} ++ args);
            } else {
                const prefix = @tagName(Scope) ++ ": ";
                zfmt(prefix ++ format, args);
            }
        }
    };
}

const c = @import("capi.zig");
pub export var logFn: c.LogFn = defaultLog;
var logBuf: [1024]u8 = undefined;

pub fn defaultLog(_: c.Str) callconv(.C) void {
    // Default log is a nop.
}

pub fn zfmt(comptime format: []const u8, args: anytype) void {
    var b = std.io.fixedBufferStream(&logBuf);
    std.fmt.format(b.writer(), format, args) catch cy.fatal();
    logFn.?(c.initStr(b.getWritten()));
}

pub fn fmt(format: []const u8, args: []const cy.fmt.FmtValue) void {
    var b = std.io.fixedBufferStream(&logBuf);
    cy.fmt.print(b.writer(), format, args);
    logFn.?(c.initStr(b.getWritten()));
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
    c_log.tracev("{s}", .{ buf });
}

pub export fn zig_log_u32(buf: [*c]const u8, val: u32) void {
    c_log.tracev("{s}: {}", .{ buf, val });
}