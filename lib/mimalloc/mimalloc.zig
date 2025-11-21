const std = @import("std");
const log = std.log.scoped(.mimalloc);

const c = @cImport({
    @cInclude("mimalloc.h");
});

pub fn collect(force: bool) void {
    c.mi_collect(force);
}

pub const Allocator = struct {
    dummy: bool,

    const vtable = std.mem.Allocator.VTable{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
        .remap = remap,
    };

    pub fn init(self: *Allocator) void {
        self.* = .{
            .dummy = false,
        };

        // Don't eagerly commit segments.
        c.mi_option_disable(c.mi_option_eager_commit);
    }

    pub fn deinit(self: *Allocator) void {
        _ = self;
    }

    pub fn allocator(self: *Allocator) std.mem.Allocator {
        return std.mem.Allocator{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    fn alloc(
        ptr: *anyopaque,
        len: usize,
        alignment: std.mem.Alignment,
        ret_addr: usize,
    ) ?[*]u8 {
        _ = ptr;
        _ = ret_addr;
        const byte_alignment = alignment.toByteUnits();
        return @ptrCast(c.mi_malloc_aligned(len, byte_alignment));
    }

    fn resize(
        ptr: *anyopaque,
        buf: []u8,
        alignment: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) bool {
        _ = ptr;
        _ = alignment;
        _ = ret_addr;
        if (new_len > buf.len) {
            if (c.mi_expand(buf.ptr, new_len)) |_| {
                return true;
            }
            return false;
        } else {
            return true;
        }
    }

    fn remap(ptr: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        _ = ptr;
        _ = memory;
        _ = alignment;
        _ = new_len;
        _ = ret_addr;
        return null;
    }

    fn free(
        ptr: *anyopaque,
        buf: []u8,
        alignment: std.mem.Alignment,
        ret_addr: usize,
    ) void {
        _ = ptr;
        _ = ret_addr;
        _ = alignment;
        c.mi_free(buf.ptr);
    }
};
