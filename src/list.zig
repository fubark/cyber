const std = @import("std");
const builtin = @import("builtin");
const debug = builtin.mode == .Debug;

const eval2 = ".eval2";

pub fn List(comptime T: type) type {
    return struct {
        buf: []T = &.{},
        end: usize = 0,

        const ListT = @This();

        pub fn deinit(self: *ListT, alloc: std.mem.Allocator) void {
            alloc.free(self.buf);
        }

        pub fn append(self: *ListT, alloc: std.mem.Allocator, val: T) linksection(eval2) !void {
            @setRuntimeSafety(debug);
            if (self.end == self.buf.len) {
                try self.growTotalCapacity(alloc, self.end + 1);
            }
            self.buf[self.end] = val;
            self.end += 1;
        }

        pub fn growTotalCapacity(self: *ListT, alloc: std.mem.Allocator, newCap: usize) !void {
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