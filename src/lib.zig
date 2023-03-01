const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const Value = cy.Value;

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
export fn cyVmAlloc(vm: *cy.UserVM, size: usize) [*]const align(8) u8 {
    const slice = vm.allocator().alignedAlloc(u8, 8, size) catch stdx.fatal();
    return slice.ptr;
}

export fn cyVmFree(vm: *cy.UserVM, ptr: [*]const align(8) u8, size: usize) void {
    vm.allocator().free(ptr[0..size]);
}

export fn cyVmEval(vm: *cy.UserVM, src: c.CStr, outVal: *cy.Value) c.CyResultCode {
    outVal.* = vm.eval("main", src.charz[0..src.len], .{
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

// comptime {
//     if (cy.isWasm) {
//     }
// }

export fn cyVmGetLastErrorReport(vm: *cy.UserVM) c.CStr {
    const report = vm.allocLastErrorReport() catch stdx.fatal();
    defer vm.internal().alloc.free(report);
    if (report.len > tempBuf.len - 1) {
        stdx.panic("Buffer too small.");
    }
    @memcpy(&tempBuf, report.ptr, report.len);
    tempBuf[report.len] = 0;
    return c.CStr{
        .charz = &tempBuf,
        .len = report.len,
    };
}

export fn cyVmAddModuleLoader(vm: *cy.UserVM, cspec: c.CStr, func: c.CyLoadModuleFunc) void {
    const absSpec = cspec.charz[0..cspec.len];
    vm.addModuleLoader(absSpec, @ptrCast(cy.ModuleLoaderFunc, func)) catch stdx.fatal();
}

export fn cyVmSetModuleFunc(vm: *cy.UserVM, mod: *cy.Module, cname: c.CStr, numParams: u32, func: c.CyFunc) void {
    const symName = cname.charz[0..cname.len];
    mod.setNativeFunc(&vm.internal().compiler, symName, numParams, @ptrCast(cy.NativeFuncPtr, func)) catch stdx.fatal();
}

export fn cyVmSetModuleVar(vm: *cy.UserVM, mod: *cy.Module, cname: c.CStr, val: c.CyValue) void {
    const symName = cname.charz[0..cname.len];
    mod.setVar(&vm.internal().compiler, symName, @bitCast(cy.Value, val)) catch stdx.fatal();
}

export fn cyVmRelease(vm: *cy.UserVM, val: Value) void {
    vm.release(val);
}

export fn cyValueNone() Value {
    return Value.None;
}

export fn cyValueTrue() Value {
    return Value.True;
}

export fn cyValueFalse() Value {
    return Value.False;
}

export fn cyValueNumber(n: f64) Value {
    return Value.initF64(n);
}

export fn cyValueGetOrAllocStringInfer(vm: *cy.UserVM, cstr: c.CStr) Value {
    return vm.allocStringInfer(cstr.charz[0..cstr.len]) catch stdx.fatal();
}

export fn cyValueGetOrAllocAstring(vm: *cy.UserVM, cstr: c.CStr) Value {
    return vm.allocAstring(cstr.charz[0..cstr.len]) catch stdx.fatal();
}

export fn cyValueGetOrAllocUstring(vm: *cy.UserVM, cstr: c.CStr, charLen: u32) Value {
    return vm.allocUstring(cstr.charz[0..cstr.len], charLen) catch stdx.fatal();
}

export fn cyValueAsDouble(val: Value) f64 {
    return val.asF64();
}