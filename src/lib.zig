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
    if (cy.isWasm) {
        stdx.log.wasm.init(alloc);
    }
    return vm;
}

export fn cyVmDestroy(vm: *cy.UserVM) void {
    vm.deinit();
    if (cy.isWasm) {
        stdx.log.wasm.deinit();
    }
}

/// This is useful when calling into wasm to allocate some memory.
export fn cyVmAlloc(vm: *cy.UserVM, size: usize) [*]const u8 {
    const slice = vm.allocator().alloc(u8, size) catch stdx.fatal();
    return slice.ptr;
}

export fn cyVmFree(vm: *cy.UserVM, ptr: [*]const u8, size: usize) void {
    vm.allocator().free(ptr[0..size]);
}

export fn cyVmEval(vm: *cy.UserVM, src: [*]const u8, srcLen: usize) cy.Value {
    return vm.eval("main", src[0..srcLen], .{
        .singleRun = false,
    }) catch |err| {
        if (err == error.Panic) {
            vm.dumpPanicStackTrace() catch stdx.fatal();
            return cy.Value.Panic;
        } else {
            stdx.panicFmt("{}", .{err});
        }
    };
}

export fn cyVmRelease(vm: *cy.UserVM, val: cy.Value) void {
    vm.release(val);
}

export fn cyValueIsPanic(val: cy.Value) bool {
    return val.isPanic();
}