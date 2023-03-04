const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const Value = cy.Value;
const t = stdx.testing;

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
    vm.init(alloc) catch fatal();
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
    const slice = vm.allocator().alignedAlloc(u8, 8, size) catch fatal();
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
    const report = vm.allocLastErrorReport() catch fatal();
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
    vm.addModuleLoader(absSpec, @ptrCast(cy.ModuleLoaderFunc, func)) catch fatal();
}

export fn cyVmSetModuleFunc(vm: *cy.UserVM, mod: *cy.Module, cname: c.CStr, numParams: u32, func: c.CyFunc) void {
    const symName = cname.charz[0..cname.len];
    mod.setNativeFunc(&vm.internal().compiler, symName, numParams, @ptrCast(cy.NativeFuncPtr, func)) catch fatal();
}

export fn cyVmSetModuleVar(vm: *cy.UserVM, mod: *cy.Module, cname: c.CStr, val: c.CyValue) void {
    const symName = cname.charz[0..cname.len];
    mod.setVar(&vm.internal().compiler, symName, @bitCast(cy.Value, val)) catch fatal();
}

export fn cyVmRelease(vm: *cy.UserVM, val: Value) void {
    vm.release(val);
}

export fn cyVmRetain(vm: *cy.UserVM, val: Value) void {
    vm.retain(val);
}

export fn cyVmGetUserData(vm: *cy.UserVM) ?*anyopaque {
    return vm.getUserData();
}

export fn cyVmSetUserData(vm: *cy.UserVM, userData: ?*anyopaque) void {
    vm.setUserData(userData);
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

export fn cyValueInteger(n: i32) Value {
    return Value.initI32(n);
}

export fn cyValueGetOrAllocStringInfer(vm: *cy.UserVM, cstr: c.CStr) Value {
    return vm.allocStringInfer(cstr.charz[0..cstr.len]) catch fatal();
}

export fn cyValueGetOrAllocAstring(vm: *cy.UserVM, cstr: c.CStr) Value {
    return vm.allocAstring(cstr.charz[0..cstr.len]) catch fatal();
}

export fn cyValueGetOrAllocUstring(vm: *cy.UserVM, cstr: c.CStr, charLen: u32) Value {
    return vm.allocUstring(cstr.charz[0..cstr.len], charLen) catch fatal();
}

export fn cyValueAllocList(vm: *cy.UserVM) Value {
    return vm.allocEmptyList() catch fatal();
}

export fn cyValueAllocMap(vm: *cy.UserVM) Value {
    return vm.allocEmptyMap() catch fatal();
}

export fn cyValueAsNumber(val: Value) f64 {
    return val.asF64();
}

test "cyValueAsNumber()" {
    try t.eq(c.cyValueAsNumber(c.cyValueNumber(123.0)), 123);
}

export fn cyValueToBool(val: Value) bool {
    return val.toBool();
}

test "cyValueToBool()" {
    try t.eq(c.cyValueToBool(c.cyValueNumber(123.0)), true);
    try t.eq(c.cyValueToBool(c.cyValueNumber(0)), true);
    try t.eq(c.cyValueToBool(c.cyValueTrue()), true);
    try t.eq(c.cyValueToBool(c.cyValueNone()), false);
    try t.eq(c.cyValueToBool(c.cyValueFalse()), false);
}

export fn cyValueAsBool(val: Value) bool {
    return val.asBool();
}

test "cyValueAsBool()" {
    try t.eq(c.cyValueAsBool(c.cyValueTrue()), true);
    try t.eq(c.cyValueAsBool(c.cyValueFalse()), false);
}

export fn cyValueAsInteger(val: Value) c_int {
    return val.asInteger();
}

test "cyValueAsInteger()" {
    try t.eq(c.cyValueAsInteger(c.cyValueInteger(123)), 123);
}

export fn cyValueAsTagLiteralId(val: Value) u32 {
    return val.asTagLiteralId();
}

export fn cyValueToTempString(vm: *cy.UserVM, val: Value) c.CStr {
    const str = vm.valueToTempString(val);
    vm.internal().u8Buf.resize(vm.allocator(), str.len + 1) catch fatal();
    @memcpy(vm.internal().u8Buf.buf.ptr, str.ptr, str.len);
    vm.internal().u8Buf.buf[str.len] = 0;
    return .{
        .charz = vm.internal().u8Buf.buf.ptr,
        .len = str.len,
    };
}

export fn cyValueToTempRawString(vm: *cy.UserVM, val: Value) c.CStr {
    const str = vm.valueToTempRawString(val);
    vm.internal().u8Buf.resize(vm.allocator(), str.len + 1) catch fatal();
    @memcpy(vm.internal().u8Buf.buf.ptr, str.ptr, str.len);
    vm.internal().u8Buf.buf[str.len] = 0;
    return .{
        .charz = vm.internal().u8Buf.buf.ptr,
        .len = str.len,
    };
}

test "Constants." {
    try t.eq(c.CY_TypeNone, cy.NoneT);
    try t.eq(c.CY_TypeBoolean, cy.BooleanT);
    try t.eq(c.CY_TypeError, cy.ErrorT);
    try t.eq(c.CY_TypeStaticAstring, cy.StaticAstringT);
    try t.eq(c.CY_TypeStaticUstring, cy.StaticUstringT);
    try t.eq(c.CY_TypeUserTag, cy.UserTagT);
    try t.eq(c.CY_TypeUserTagLiteral, cy.UserTagLiteralT);
    try t.eq(c.CY_TypeInteger, cy.IntegerT);
    try t.eq(c.CY_TypeNumber, cy.NumberT);
    try t.eq(c.CY_TypeList, cy.ListS);
    try t.eq(c.CY_TypeListIter, cy.ListIteratorT);
    try t.eq(c.CY_TypeMap, cy.MapS);
    try t.eq(c.CY_TypeMapIter, cy.MapIteratorT);
    try t.eq(c.CY_TypeClosure, cy.ClosureS);
    try t.eq(c.CY_TypeLambda, cy.LambdaS);
    try t.eq(c.CY_TypeAstring, cy.AstringT);
    try t.eq(c.CY_TypeUstring, cy.UstringT);
    try t.eq(c.CY_TypeStringSlice, cy.StringSliceT);
    try t.eq(c.CY_TypeRawString, cy.RawStringT);
    try t.eq(c.CY_TypeRawStringSlice, cy.RawStringSliceT);
    try t.eq(c.CY_TypeFiber, cy.FiberS);
    try t.eq(c.CY_TypeBox, cy.BoxS);
    try t.eq(c.CY_TypeNativeFunc1, cy.NativeFunc1S);
    try t.eq(c.CY_TypeTccState, cy.TccStateS);
    try t.eq(c.CY_TypeOpaquePtr, cy.OpaquePtrS);
    try t.eq(c.CY_TypeFile, cy.FileT);
    try t.eq(c.CY_TypeDir, cy.DirT);
    try t.eq(c.CY_TypeDirIter, cy.DirIteratorT);
    try t.eq(c.CY_TypeSymbol, cy.SymbolT);
}

export fn cyValueGetTypeId(val: Value) c.CyTypeId {
    return val.getTypeId();
}

test "cyValueGetType()" {
    try t.eq(c.cyValueGetTypeId(c.cyValueNumber(123)), cy.NumberT);
}