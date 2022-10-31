const std = @import("std");
const stdx = @import("../stdx.zig");
const builtin = @import("builtin");
const debug = builtin.mode == .Debug;

/// Stack has more performant operations than std.ArrayList by using a top var.
pub fn Stack(comptime T: type) type {
    return struct {
        top: usize = 0,
        buf: []T = &.{},

        const StackT = @This();

        pub fn deinit(self: *StackT, alloc: std.mem.Allocator) void {
            alloc.free(self.buf);
        }

        pub fn push(self: *StackT, alloc: std.mem.Allocator, val: T) !void {
            @setRuntimeSafety(debug);
            if (self.top == self.buf.len) {
                try self.growTotalCapacity(alloc, self.top + 1);
            }
            self.buf[self.top] = val;
            self.top += 1;
        }

        pub fn pushSliceNoCheck(self: *StackT, vals: []const T) void {
            @setRuntimeSafety(debug);
            var i: u32 = 0;
            while (i < vals.len) : (i += 1) {
                self.buf[self.top + i] = vals[i];
            }
            self.top += @intCast(u32, vals.len);
        }

        pub inline fn pop(self: *StackT) T {
            @setRuntimeSafety(debug);
            self.top -= 1;
            return self.buf[self.top];
        }

        pub inline fn clearRetainingCapacity(self: *StackT) void {
            self.top = 0;
        }

        pub inline fn ensureUnusedCapacity(self: *StackT, alloc: std.mem.Allocator, unused: usize) !void {
            const newCap = self.top + unused;
            return self.ensureTotalCapacity(alloc, newCap);
        }

        pub fn ensureTotalCapacity(self: *StackT, alloc: std.mem.Allocator, newCap: usize) !void {
            if (newCap > self.buf.len) {
                try self.growTotalCapacity(alloc, newCap);
            }
        }

        pub fn growTotalCapacity(self: *StackT, alloc: std.mem.Allocator, newCap: usize) !void {
            var betterCap = newCap;
            while (true) {
                betterCap +|= betterCap / 2 + 8;
                if (betterCap >= newCap) {
                    break;
                }
            }
            self.buf = try alloc.reallocAtLeast(self.buf, betterCap);
        }
    };
}