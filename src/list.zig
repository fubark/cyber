const std = @import("std");
const builtin = @import("builtin");
const debug = builtin.mode == .Debug;

const section = @import("cyber.zig").HotSection;

pub fn List(comptime T: type) type {
    return struct {
        buf: []T = &.{},
        len: usize = 0,

        const ListT = @This();

        pub fn deinit(self: *ListT, alloc: std.mem.Allocator) void {
            alloc.free(self.buf);
        }

        pub fn append(self: *ListT, alloc: std.mem.Allocator, val: T) linksection(section) !void {
            @setRuntimeSafety(debug);
            if (self.len == self.buf.len) {
                try self.growTotalCapacity(alloc, self.len + 1);
            }
            self.buf[self.len] = val;
            self.len += 1;
        }

        pub fn remove(self: *ListT, idx: usize) linksection(section) void {
            std.mem.copy(T, self.buf[idx..self.len-1], self.buf[idx+1..self.len]);
            self.len -= 1;
        }

        pub fn insertAssumeCapacity(self: *ListT, idx: usize, val: T) linksection(section) void {
            std.mem.copyBackwards(T, self.buf[idx+1..self.len+1], self.buf[idx..self.len]);
            self.buf[idx] = val;
            self.len += 1;
        }

        pub fn appendAssumeCapacity(self: *ListT, val: T) linksection(section) void {
            @setRuntimeSafety(debug);
            self.buf[self.len] = val;
            self.len += 1;
        }

        pub fn appendSlice(self: *ListT, alloc: std.mem.Allocator, slice: []const T) linksection(section) !void {
            @setRuntimeSafety(debug);
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
                self.buf = try alloc.alloc(T, newCap);
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
    };
}

const Writer = struct {
    list: *List(u8),
    alloc: std.mem.Allocator,

    const WriterT = @This();

    pub fn write(self: WriterT, data: []const u8) linksection(section) Error!usize {
        try self.list.appendSlice(self.alloc, data);
        return data.len;
    }

    pub fn writeAll(self: WriterT, data: []const u8) linksection(section) Error!void {
        _ = try self.write(data);
    }

    pub fn writeByteNTimes(self: WriterT, byte: u8, n: usize) linksection(section) Error!void {
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