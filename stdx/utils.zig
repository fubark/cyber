const std = @import("std");

/// @alignCast seems to be broken (does not insert runtime checks) when passed a comptime int from an expression like
/// @typeInfo(Ptr).Pointer.alignment (for cases where Ptr has a custom alignment, eg. *align(1) u32).
/// Current fix is to branch to common alignments.
pub inline fn ptrCastAlign(comptime Ptr: type, ptr: anytype) Ptr {
    const alignment = comptime @typeInfo(Ptr).Pointer.alignment;
    return comptime switch (alignment) {
        0 => @ptrCast(Ptr, ptr),
        1 => @ptrCast(Ptr, @alignCast(1, ptr)),
        2 => @ptrCast(Ptr, @alignCast(2, ptr)),
        4 => @ptrCast(Ptr, @alignCast(4, ptr)),
        8 => @ptrCast(Ptr, @alignCast(8, ptr)),
        else => unreachable,
    };
}

// Shared opaque type.
pub const Opaque = opaque {
    pub fn fromPtr(comptime T: type, ptr: T) *Opaque {
        return @ptrCast(*Opaque, ptr);
    }

    pub fn toPtr(comptime T: type, ptr: *Opaque) T {
        return @intToPtr(T, @ptrToInt(ptr));
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

pub const SizedPtr = struct {
    const Self = @This();

    ptr: *Opaque,
    size: u32,

    pub fn init(ptr: anytype) Self {
        const Ptr = @TypeOf(ptr);
        return .{
            .ptr = Opaque.fromPtr(Ptr, ptr),
            .size = @sizeOf(@typeInfo(Ptr).Pointer.child),
        };
    }

    pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        const slice = Opaque.toPtr([*]u8, self.ptr)[0..self.size];
        alloc.free(slice);
    }
};

/// The ptr or slice inside should be freed depending on the flag.
/// Use case: Sometimes a branching condition returns heap or non-heap memory. The deinit logic can just skip non-heap memory.
pub fn MaybeOwned(comptime PtrOrSlice: type) type {
    return struct {
        inner: PtrOrSlice,
        owned: bool,

        pub fn initOwned(inner: PtrOrSlice) @This() {
            return .{
                .inner = inner,
                .owned = true,
            };
        }

        pub fn initUnowned(inner: PtrOrSlice) @This() {
            return .{
                .inner = inner,
                .owned = false,
            };
        }

        pub fn deinit(self: @This(), alloc: std.mem.Allocator) void {
            if (self.owned) {
                if (@typeInfo(PtrOrSlice) == .Pointer and @typeInfo(PtrOrSlice).Pointer.size == .Slice) {
                    alloc.free(self.inner);
                } else {
                    alloc.destroy(self.inner);
                }
            }
        }
    };
}

// Stores allocator with data ptr.
pub fn Box(comptime T: type) type {
    if (@typeInfo(T) == .Pointer and @typeInfo(T).Pointer.size == .Slice) {
        return BoxSlice(T);
    } else if (@typeInfo(T) == .Struct) {
        return BoxPtr(T);
    }
    @compileError("not supported");
}

fn BoxSlice(comptime T: type) type {
    return struct {
        const Self = @This();

        slice: T,
        alloc: std.mem.Allocator,

        pub fn init(alloc: std.mem.Allocator, slice: T) Self {
            return .{
                .alloc = alloc,
                .slice = slice,
            };
        }

        pub fn deinit(self: *const Self) void {
            self.alloc.free(self.slice);
        }
    };
}

fn BoxPtr(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: *T,
        alloc: std.mem.Allocator,

        pub fn init(alloc: std.mem.Allocator, ptr: *T) Self {
            return .{
                .alloc = alloc,
                .ptr = ptr,
            };
        }

        pub fn create(alloc: std.mem.Allocator) !Self {
            return Self{
                .alloc = alloc,
                .ptr = try alloc.create(T),
            };
        }

        pub fn createInit(alloc: std.mem.Allocator, _init: T) !Self {
            const new = Self{
                .alloc = alloc,
                .ptr = try alloc.create(T),
            };
            new.ptr.* = _init;
            return new;
        }

        pub fn deinit(self: *Self) void {
            // Contained item deinit is called if it exists.
            if (@hasDecl(T, "deinit")) {
                self.ptr.deinit();
            }
            self.alloc.destroy(self.ptr);
        }

        pub fn deinitOuter(self: *Self) void {
            self.alloc.destroy(self.ptr);
        }

        pub fn toSized(self: Self) SizedBox {
            return .{
                .ptr = Opaque.fromPtr(*T, self.ptr),
                .size = @sizeOf(T),
                .alloc = self.alloc,
            };
        }
    };
}

pub const SizedBox = struct {
    const Self = @This();

    ptr: *Opaque,
    alloc: std.mem.Allocator,
    size: u32,

    pub fn deinit(self: *const Self) void {
        const slice = Opaque.toPtr([*]u8, self.ptr)[0..self.size];
        self.alloc.free(slice);
    }
};

pub fn Pair(comptime T1: type, comptime T2: type) type {
    return struct {
        first: T1,
        second: T2,
        
        pub fn init(first: T1, second: T2) @This() {
            return .{ .first = first, .second = second };
        }
    };
}