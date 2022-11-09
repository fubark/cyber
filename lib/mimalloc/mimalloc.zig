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
        alignment: u29,
        len_align: u29,
        ret_addr: usize,
    ) std.mem.Allocator.Error![]u8 {
        _ = ret_addr;
        _ = ptr;
        // const self = @ptrCast(*Allocator, @alignCast(@sizeOf(usize), ptr));
        if (c.mi_malloc_aligned(len, alignment)) |userPtr| {
            if (len_align == 0) {
                return @ptrCast([*]u8, userPtr)[0..len];
            }
            return @ptrCast([*]u8, userPtr)[0..std.mem.alignBackwardAnyAlign(len, len_align)];
        } else return error.OutOfMemory;
    }

    fn resize(
        ptr: *anyopaque,
        buf: []u8,
        buf_align: u29,
        new_len: usize,
        len_align: u29,
        ret_addr: usize,
    ) ?usize {
        _ = ptr;
        _ = len_align;
        _ = ret_addr;
        _ = buf_align;
        if (new_len > buf.len) {
            const available = c.mi_usable_size(buf.ptr);
            if (available > new_len) {
                if (c.mi_expand(buf.ptr, new_len)) |_| {
                    return new_len;
                }
            }
            return null;
        } else {
            return new_len;
        }
    }

    fn free(
        ptr: *anyopaque,
        buf: []u8,
        buf_align: u29,
        ret_addr: usize,
    ) void {
        _ = ret_addr;
        _ = buf_align;
        _ = ptr;
        // const self = @ptrCast(*Allocator, @alignCast(@sizeOf(usize), ptr));
        c.mi_free(buf.ptr);
    }
};