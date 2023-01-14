const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const builtin = @import("builtin");
const debug = builtin.mode == .Debug;

const cy = @import("cyber.zig");

pub fn List(comptime T: type) type {
    return ListAligned(T, null);
}

pub fn ListAligned(comptime T: type, comptime Align: ?u29) type {
    return struct {
        buf: if (Align) |a| ([]align(a) T) else []T = &.{},
        len: usize = 0,

        const ListT = @This();

        pub fn deinit(self: *ListT, alloc: std.mem.Allocator) void {
            alloc.free(self.buf);
        }

        pub inline fn clearRetainingCapacity(self: *ListT) void {
            self.len = 0;
        }

        pub fn append(self: *ListT, alloc: std.mem.Allocator, val: T) linksection(cy.Section) !void {
            if (self.len == self.buf.len) {
                try self.growTotalCapacity(alloc, self.len + 1);
            }
            self.buf[self.len] = val;
            self.len += 1;
        }

        pub fn remove(self: *ListT, idx: usize) linksection(cy.Section) void {
            std.mem.copy(T, self.buf[idx..self.len-1], self.buf[idx+1..self.len]);
            self.len -= 1;
        }

        pub fn insertAssumeCapacity(self: *ListT, idx: usize, val: T) linksection(cy.Section) void {
            std.mem.copyBackwards(T, self.buf[idx+1..self.len+1], self.buf[idx..self.len]);
            self.buf[idx] = val;
            self.len += 1;
        }

        pub fn appendAssumeCapacity(self: *ListT, val: T) linksection(cy.Section) void {
            self.buf[self.len] = val;
            self.len += 1;
        }

        pub fn appendSlice(self: *ListT, alloc: std.mem.Allocator, slice: []const T) linksection(cy.Section) !void {
            try self.ensureTotalCapacity(alloc, self.len + slice.len);
            const oldLen = self.len;
            self.len += slice.len;
            std.mem.copy(T, self.buf[oldLen..self.len], slice);
        }

        pub inline fn writer(self: *ListT, alloc: std.mem.Allocator) Writer {
            if (T != u8) {
                @compileError("The Writer interface is only defined for List(u8) " ++
                    "but the given type is ArrayList(" ++ @typeName(T) ++ ")");
            }
            return Writer{
                .list = self,
                .alloc = alloc,
            };
        }

        pub inline fn items(self: *const ListT) []T {
            return self.buf[0..self.len];
        }

        pub fn resize(self: *ListT, alloc: std.mem.Allocator, len: usize) !void {
            try self.ensureTotalCapacity(alloc, len);
            self.len = len;
        }

        pub inline fn ensureMaxCapOrClear(self: *ListT, alloc: std.mem.Allocator, maxCap: usize) linksection(cy.Section) !void {
            if (maxCap < self.buf.len) {
                alloc.free(self.buf);
                self.buf = try alloc.alignedAlloc(T, Align, maxCap);
                self.len = 0;
            }
        }

        pub inline fn ensureTotalCapacity(self: *ListT, alloc: std.mem.Allocator, newCap: usize) !void {
            if (newCap > self.buf.len) {
                try self.growTotalCapacity(alloc, newCap);
            }
        }

        pub inline fn ensureTotalCapacityPrecise(self: *ListT, alloc: std.mem.Allocator, newCap: usize) !void {
            if (newCap > self.buf.len) {
                try self.growTotalCapacityPrecise(alloc, newCap);
            }
        }

        pub fn growTotalCapacityPrecise(self: *ListT, alloc: std.mem.Allocator, newCap: usize) !void {
            if (alloc.resize(self.buf, newCap)) {
                self.buf.len = newCap;
            } else {
                const old = self.buf;
                self.buf = try alloc.alignedAlloc(T, Align, newCap);
                std.mem.copy(T, self.buf[0..self.len], old[0..self.len]);
                alloc.free(old);
            }
        }

        pub fn growTotalCapacity(self: *ListT, alloc: std.mem.Allocator, newCap: usize) !void {
            var betterCap = self.buf.len;
            while (true) {
                betterCap +|= betterCap / 2 + 8;
                if (betterCap >= newCap) {
                    break;
                }
            }
            try self.growTotalCapacityPrecise(alloc, betterCap);
        }

        const Writer = struct {
            list: *ListT,
            alloc: std.mem.Allocator,

            const WriterT = @This();

            pub inline fn pos(self: WriterT) usize {
                return self.list.len;
            }

            pub inline fn sliceFrom(self: WriterT, idx: usize) []const u8 {
                return self.list.buf[idx..self.list.len];
            }

            pub fn write(self: WriterT, data: []const u8) linksection(cy.Section) Error!usize {
                try self.list.appendSlice(self.alloc, data);
                return data.len;
            }

            pub fn writeAll(self: WriterT, data: []const u8) linksection(cy.Section) Error!void {
                _ = try self.write(data);
            }

            pub fn writeByteNTimes(self: WriterT, byte: u8, n: usize) linksection(cy.Section) Error!void {
                var bytes: [256]u8 = undefined;
                std.mem.set(u8, bytes[0..], byte);

                var remaining = n;
                while (remaining > 0) {
                    const to_write = std.math.min(remaining, bytes.len);
                    try self.writeAll(bytes[0..to_write]);
                    remaining -= to_write;
                }
            }

            pub const Error = error{OutOfMemory};
        };
    };
}

test "List." {
    var l: List(u8) = .{};
    defer l.deinit(t.alloc);

    try l.append(t.alloc, 'a');
    try l.appendSlice(t.alloc, "foobar");
    try l.append(t.alloc, 'z');

    try t.eq(l.len, 8);
    try t.eqStr(l.items(), "afoobarz");
}