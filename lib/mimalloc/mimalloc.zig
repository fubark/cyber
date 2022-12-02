const std = @import("std");
const log = std.log.scoped(.mimalloc);

const c = @cImport({
    @cInclude("mimalloc.h");
});

pub usingnamespace c;

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
        _ = ret_addr;
        _ = ptr;
        const alignment = @as(usize, 1) << @intCast(u6, log2_align);
        return @ptrCast(?[*]u8, c.mi_malloc_aligned(len, alignment));
    }

    fn resize(
        ptr: *anyopaque,
        buf: []u8,
        log2_align: u8,
        new_len: usize,
        ret_addr: usize,
    ) bool {
        _ = ptr;
        _ = ret_addr;
        _ = log2_align;
        if (new_len > buf.len) {
            const available = c.mi_usable_size(buf.ptr);
            if (available > new_len) {
                if (c.mi_expand(buf.ptr, new_len)) |_| {
                    return true;
                }
            }
            return false;
        } else {
            return true;
        }
    }

    fn free(
        ptr: *anyopaque,
        buf: []u8,
        log2_align: u8,
        ret_addr: usize,
    ) void {
        _ = ret_addr;
        _ = log2_align;
        _ = ptr;
        // const self = @ptrCast(*Allocator, @alignCast(@sizeOf(usize), ptr));
        c.mi_free(buf.ptr);
    }
};