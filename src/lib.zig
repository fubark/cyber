const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const mi = @import("mimalloc");

const cy = @import("cyber.zig");

/// Use mimalloc for fast builds.
const UseMimalloc = builtin.mode == .ReleaseFast and false;

var gpa: std.heap.GeneralPurposeAllocator(.{
    .enable_memory_limit = false,
    .stack_trace_frames = if (builtin.mode == .Debug) 10 else 0,
}) = .{};
var miAlloc: mi.Allocator = undefined;

export fn cyVmCreate() *cy.UserVM {
    var alloc: std.mem.Allocator = undefined;
    if (UseMimalloc) {
        miAlloc.init();
        alloc = miAlloc.allocator();
    } else {
        if (cy.isWasm) {
            alloc = std.heap.wasm_allocator;
        } else {
            alloc = gpa.allocator();
        }
    }

    const vm = cy.getUserVM();
    vm.init(alloc) catch stdx.fatal();
    return vm;
}

export fn cyVmDestroy(vm: *cy.UserVM) void {
    vm.deinit();
}

export fn cyVmEval(vm: *cy.UserVM, src: [*]const u8, srcLen: usize) cy.Value {
    return vm.eval("main", src[0..srcLen], .{
        .singleRun = false,
    }) catch stdx.fatal();
}