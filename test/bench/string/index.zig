const std = @import("std");

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const base = "abcdefghijklmnopqrstuvwxyz123456";
    const str = try alloc.alloc(u8, base.len * 1000000 + "waldo".len);
    var i: u32 = 0;
    while (i < 1000000) : (i += 1) {
        @memcpy(str.ptr + i * base.len, base, base.len);
    }
    @memcpy(str.ptr + 1000000 * base.len, "waldo", "waldo".len);

    const start = std.time.milliTimestamp();
    var idx: usize = 0;
    i = 0;
    while (i < 50) : (i += 1) {
        idx = std.mem.indexOf(u8, str, "waldo") orelse @panic("error");
    }
    std.debug.print("idx: {} ms: {}\n", .{idx, std.time.milliTimestamp() - start});
}