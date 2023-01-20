const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx.zig");

extern "stdx" fn jsWarn(ptr: [*]const u8, len: usize) void;
pub extern "stdx" fn jsLog(ptr: [*]const u8, len: usize) void;
extern "stdx" fn jsErr(ptr: [*]const u8, len: usize) void;

const DebugLog = builtin.mode == .Debug and true;

/// Fixed sized scratch buffer for formatting logs.
/// This allows small logs to still work without an allocator.
var small_buf: [1024]u8 = undefined;

/// Larger logs will use a growing heap allocation.
var large_buf_inner: std.ArrayList(u8) = undefined;
var large_buf: *std.ArrayList(u8) = undefined;

pub fn init(alloc: std.mem.Allocator) void {
    large_buf_inner = std.ArrayList(u8).init(alloc);
    large_buf = &large_buf_inner;
}

pub fn deinit() void {
    large_buf.deinit();
}

pub fn scoped(comptime Scope: @Type(.EnumLiteral)) type {
    return struct {
        pub fn debug(
            comptime format: []const u8,
            args: anytype,
        ) void {
            if (DebugLog) {
                const prefix = if (Scope == .default) "debug: " else "(" ++ @tagName(Scope) ++ "): ";
                const str = fmt(prefix, format, args);
                jsLog(str.ptr, str.len);
            }
        }

        pub fn info(
            comptime format: []const u8,
            args: anytype,
        ) void {
            const prefix = if (Scope == .default) "info: " else "(" ++ @tagName(Scope) ++ "): ";
            const str = fmt(prefix, format, args);
            jsLog(str.ptr, str.len);
        }

        pub fn warn(
            comptime format: []const u8,
            args: anytype,
        ) void {
            const prefix = if (Scope == .default) "warn: " else "(" ++ @tagName(Scope) ++ "): ";
            const str = fmt(prefix, format, args);
            jsWarn(str.ptr, str.len);
        }

        pub fn err(
            comptime format: []const u8,
            args: anytype,
        ) void {
            const prefix = if (Scope == .default) "err: " else "(" ++ @tagName(Scope) ++ "): ";
            const str = fmt(prefix, format, args);
            jsErr(str.ptr, str.len);
        }

        inline fn fmt(comptime prefix: []const u8, comptime format: []const u8, args: anytype) []const u8 {
            const len = @intCast(usize, std.fmt.count(prefix ++ format, args));
            if (len <= small_buf.len) {
                return std.fmt.bufPrint(&small_buf, prefix ++ format, args) catch @panic("error");
            } else {
                large_buf.clearRetainingCapacity();
                const writer = large_buf.writer();
                std.fmt.format(writer, prefix ++ format, args) catch @panic("error");
                return large_buf.items[0..len];
            }
        }
    };
}
