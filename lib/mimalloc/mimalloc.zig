const std = @import("std");
const log = std.log.scoped(.mimalloc);

const c = @cImport({
    @cInclude("mimalloc.h");
});

pub usingnamespace c;

pub fn collect(force: bool) void {
    c.mi_collect(force);
}

const kb = 1024;
const BigSize = 4 * kb;

const usizeShiftInt = std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(usize)));

// Uses page allocator for objects > 4kb, otherwise mimalloc is used.
pub const Allocator = struct {
    dummy: bool,

    const vtable = std.mem.Allocator.VTable{
        .alloc = alloc,
        .resize = resize,
        .free = free,
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
        log2_align: u8,
        ret_addr: usize,
    ) ?[*]u8 {
        _ = ptr;
        const alignment = @as(usize, 1) << @intCast(usizeShiftInt, log2_align);
        const effLen = len + alignment - 1;
        if (effLen > BigSize) {
            return std.heap.page_allocator.rawAlloc(len, log2_align, ret_addr);
        } else {
            return @ptrCast(?[*]u8, c.mi_malloc_aligned(len, alignment));
        }
    }

    fn resize(
        ptr: *anyopaque,
        buf: []u8,
        log2_align: u8,
        new_len: usize,
        ret_addr: usize,
    ) bool {
        _ = ptr;
        const alignment = @as(usize, 1) << @intCast(usizeShiftInt, log2_align);
        const effLen = buf.len + alignment - 1;
        if (effLen > BigSize) {
            if (new_len > buf.len) {
                return std.heap.page_allocator.rawResize(buf, log2_align, new_len, ret_addr);
            } else if (new_len == buf.len) {
                return true;
            } else {
                return false;
            }
        } else {
            if (new_len > buf.len) {
                if (c.mi_expand(buf.ptr, new_len)) |_| {
                    return true;
                }
                return false;
            } else {
                return true;
            }
        }
    }

    fn free(
        ptr: *anyopaque,
        buf: []u8,
        log2_align: u8,
        ret_addr: usize,
    ) void {
        _ = ptr;
        const alignment = @as(usize, 1) << @intCast(usizeShiftInt, log2_align);
        const effLen = buf.len + alignment - 1;
        if (effLen > BigSize) {
            std.heap.page_allocator.rawFree(buf, log2_align, ret_addr);
        } else {
            c.mi_free(buf.ptr);
        }
    }
};