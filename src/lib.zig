const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const mi = @import("mimalloc");
const cy = @import("cyber.zig");

const c = @cImport({
    @cInclude("cyber.h");
});

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

export fn cyVmEval(vm: *cy.UserVM, src: [*]const u8, srcLen: usize, outVal: *cy.Value) c.ResultCode {
    outVal.* = vm.eval("main", src[0..srcLen], .{
        .singleRun = false,
    }) catch |err| {
        switch (err) {
            error.TokenError => {
                return c.CY_ErrorToken;
            },
            error.ParseError => {
                return c.CY_ErrorParse;
            },
            error.CompileError => {
                return c.CY_ErrorCompile;
            },
            error.Panic => {
                return c.CY_ErrorPanic;
            },
            else => {
                return c.CY_ErrorUnknown;
            },
        }
    };
    return c.CY_Success;
}

var tempBuf: [1024]u8 align(8) = undefined;

comptime {
    if (cy.isWasm) {
        @export(cyWasmGetTempBuf, .{ .name = "cyWasmGetTempBuf" });
    }
}

fn cyWasmGetTempBuf() callconv(.C) [*]const u8 {
    return &tempBuf;
}

export fn cyVmGetLastErrorReport(vm: *cy.UserVM) [*:0]const u8 {
    const report = vm.allocLastErrorReport() catch stdx.fatal();
    defer vm.internal().alloc.free(report);
    if (report.len > tempBuf.len - 1) {
        stdx.panic("Buffer too small.");
    }
    @memcpy(&tempBuf, report.ptr, report.len);
    tempBuf[report.len] = 0;
    return @ptrCast([*:0]const u8, &tempBuf);
}

export fn cyVmRelease(vm: *cy.UserVM, val: cy.Value) void {
    vm.release(val);
}