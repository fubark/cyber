const std = @import("std");
const builtin = @import("builtin");
const cy = @import("all").cy;
const C = cy.C;

/// Template for a custom test runner.
pub fn main() !void {
    var out = std.fs.File.stdout().writer(&.{});

    cy.debug.attachSegfaultHandler(sig_handler);

    for (builtin.test_functions) |t| {
        t.func() catch |err| {
            try out.interface.print("{s} fail: {}\n", .{t.name, err});
            continue;
        };
        try out.interface.print("{s} passed\n", .{t.name});
    }

    // Deinit test allocator to surface leaks.
    _ = std.testing.allocator_instance.deinit();
}

extern var test_vm: ?*C.VM;

fn sig_handler(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*anyopaque) callconv(.c) noreturn {
    if (test_vm) |vm| {
        cy.debug.vm_segv_handler(@ptrCast(@alignCast(vm))) catch |err| {
            std.debug.panic("failed during segfault: {}", .{err});
        };
    }
    cy.debug.handleSegfaultPosix(sig, info, ctx_ptr);
}

fn panic_handler() !void {
    if (test_vm) |vm| {
        try cy.debug.vm_panic_handler(@ptrCast(@alignCast(vm)));
    }
}

pub fn panic(msg: []const u8, trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    _ = trace;
    _ = ret_addr;
    cy.debug.defaultPanic(msg, @returnAddress(), panic_handler);
}