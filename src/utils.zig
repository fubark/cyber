const std = @import("std");
const builtin = @import("builtin");

const cy = @import("cyber.zig");
const log = cy.log.scoped(.utils);

pub fn dassert(pred: bool) void {
    if (builtin.mode == .Debug) {
        std.debug.assert(pred);
    }
}

pub fn panicFmt(comptime format: []const u8, args: anytype) noreturn {
    if (cy.isWasm) {
        cy.debug.log(format, args);
    }
    std.debug.panic(format, args);
}

pub fn panic(comptime msg: []const u8) noreturn {
    if (cy.isWasm) {
        // @panic doesn't know how to print in wasm.
        cy.debug.log("{s}", .{msg});
    }
    @panic(msg);
}

pub fn getHashMapMemSize(comptime K: type, comptime V: type, cap: usize) usize {
    const Header = struct {
        values: [*]V,
        keys: [*]K,
        capacity: std.AutoHashMap(K, V).Unmanaged.Size,
    };

    const Metadata = packed struct {
        const FingerPrint = u7;
        fingerprint: FingerPrint,
        used: u1,
    };

    const header_align = @alignOf(Header);
    const key_align = if (@sizeOf(K) == 0) 1 else @alignOf(K);
    const val_align = if (@sizeOf(V) == 0) 1 else @alignOf(V);
    const max_align = comptime @max(header_align, key_align, val_align);

    const meta_size = @sizeOf(Header) + cap * @sizeOf(Metadata);

    const keys_start = std.mem.alignForward(usize, meta_size, key_align);
    const keys_end = keys_start + cap * @sizeOf(K);

    const vals_start = std.mem.alignForward(usize, keys_end, val_align);
    const vals_end = vals_start + cap * @sizeOf(V);

    return std.mem.alignForward(usize, vals_end, max_align);
}

pub inline fn ptrAlignCast(comptime Ptr: type, ptr: anytype) Ptr {
    return @ptrCast(@alignCast(ptr));
}

// Shared opaque type.
pub const Opaque = opaque {
    pub fn fromPtr(comptime T: type, ptr: T) *Opaque {
        return @ptrCast(ptr);
    }

    pub fn toPtr(comptime T: type, ptr: *Opaque) T {
        return @ptrFromInt(@intFromPtr(ptr));
    }
};

/// Slice that holds indexes instead of pointers. Use to reference slice in a growing memory buffer.
pub fn IndexSlice(comptime T: type) type {
    return struct {
        start: T,
        end: T,

        pub fn init(start: T, end: T) @This() {
            return .{
                .start = start,
                .end = end,
            };
        }

        pub fn len(self: @This()) T {
            return self.end - self.start;
        }

        pub fn isEmpty(self: @This()) bool {
            return self.start == self.end;
        }
    };
}

pub fn Pair(comptime T1: type, comptime T2: type) type {
    return struct {
        first: T1,
        second: T2,
        
        pub fn init(first: T1, second: T2) @This() {
            return .{ .first = first, .second = second };
        }
    };
}