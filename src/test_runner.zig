const std = @import("std");
const builtin = @import("builtin");

/// Template for a custom test runner.
pub fn main() !void {
    var out = std.fs.File.stdout().writer(&.{});

    test_init();

    var passed: usize = 0;
    for (builtin.test_functions) |t| {
        t.func() catch |err| {
            try out.interface.print("{s} fail: {}\n", .{t.name, err});
            if (@errorReturnTrace()) |trace| {
                std.debug.dumpStackTrace(trace.*);
            }
            continue;
        };
        passed += 1;
        try out.interface.print("{s} passed\n", .{t.name});
    }

    if (passed < builtin.test_functions.len) {
        try out.interface.print("Failed. {}/{} tests passed\n", .{passed, builtin.test_functions.len});
        return error.Failed;
    } else {
        try out.interface.print("Ok. {} tests passed\n", .{passed});
    }

    // Deinit test allocator to surface leaks.
    _ = std.testing.allocator_instance.deinit();
}

extern fn test_init() void;

extern fn zpanic(msg: [*]const u8, msg_len: usize, first_trace_addr: usize) noreturn;

pub fn panic(msg: []const u8, trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    _ = trace;
    _ = ret_addr;
    zpanic(msg.ptr, msg.len, @returnAddress());
}