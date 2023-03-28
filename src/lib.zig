const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const Value = cy.Value;
const t = stdx.testing;
const log = stdx.log.scoped(.lib);
const bt = cy.types.BuiltinTypeSymIds;

const c = @cImport({
    @cInclude("cyber.h");
});

export fn cyVmCreate() *cy.UserVM {
    const alloc = cy.heap.getAllocator();
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

export fn cyVmValidate(vm: *cy.UserVM, src: c.CStr) c.CyResultCode {
    const res = vm.internal().validate("main", src.charz[0..src.len], .{}) catch |err| {
        log.debug("validate error: {}", .{err});
        return c.CY_ErrorUnknown;
    };
    if (res.err) |err| {
        switch (err) {
            .tokenize => {
                return c.CY_ErrorToken;
            },
            .parse => {
                return c.CY_ErrorParse;
            },
            .compile => {
                return c.CY_ErrorCompile;
            },
        }
    }
    return c.CY_Success;
}

test "cyVmValidate()" {
    const vm = c.cyVmCreate();
    defer c.cyVmDestroy(vm);

    cy.silentError = true;
    defer cy.silentError = false;

    var res = c.cyVmValidate(vm, initCStr("1 + 2"));
    try t.eq(res, c.CY_Success);

    res = c.cyVmValidate(vm, initCStr("1 +"));
    try t.eq(res, c.CY_ErrorParse);
}

var tempBuf: [1024]u8 align(8) = undefined;

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
    mod.setNativeFuncExt(&vm.internal().compiler, symName, true, numParams, @ptrCast(cy.NativeFuncPtr, func)) catch fatal();
}

export fn cyVmSetModuleVar(vm: *cy.UserVM, mod: *cy.Module, cname: c.CStr, val: c.CyValue) void {
    const symName = cname.charz[0..cname.len];
    mod.setVarExt(&vm.internal().compiler, symName, true, bt.Any, @bitCast(cy.Value, val)) catch fatal();
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

export fn cyValueAllocNativeFunc(vm: *cy.UserVM, func: c.CyFunc, numParams: u32) Value {
    const rFuncSigId = vm.ensureUntypedFuncSig(numParams) catch fatal();
    return cy.heap.allocNativeFunc1(vm.internal(), @ptrCast(cy.NativeFuncPtr, func), numParams, rFuncSigId, null) catch fatal();
}

export fn cyValueTagLiteral(vm: *cy.UserVM, str: c.CStr) Value {
    const id = vm.internal().ensureSymbolExt(str.charz[0..str.len], true) catch fatal();
    return Value.initSymbol(@intCast(u8, id));
}

test "cyValueAllocNativeFunc()" {
    const vm = c.cyVmCreate();
    defer c.cyVmDestroy(vm);

    const val = c.cyValueAllocNativeFunc(vm, @intToPtr(c.CyFunc, 0), 2);
    try t.eq(c.cyValueGetTypeId(val), cy.NativeFunc1S);
}

export fn cyValueAllocPointer(vm: *cy.UserVM, ptr: ?*anyopaque) Value {
    return cy.heap.allocPointer(vm.internal(), ptr) catch fatal();
}

test "cyValueAllocPointer()" {
    const vm = c.cyVmCreate();
    defer c.cyVmDestroy(vm);

    const val = c.cyValueAllocPointer(vm, @intToPtr(?*anyopaque, 123));
    try t.eq(c.cyValueGetTypeId(val), cy.PointerT);
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
    return val.asSymbolId();
}

test "cyValueAsTagLiteralId()" {
    const vm = c.cyVmCreate();
    defer c.cyVmDestroy(vm);

    var str = initCStr("foo");
    var val = c.cyValueTagLiteral(vm, str);
    try t.eq(c.cyValueAsTagLiteralId(val), 0);

    str = initCStr("bar");
    val = c.cyValueTagLiteral(vm, str);
    try t.eq(c.cyValueAsTagLiteralId(val), 1);

    str = initCStr("foo");
    val = c.cyValueTagLiteral(vm, str);
    try t.eq(c.cyValueAsTagLiteralId(val), 0);
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
    try t.eq(c.CY_TypeEnum, cy.EnumT);
    try t.eq(c.CY_TypeSymbol, cy.SymbolT);
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
    try t.eq(c.CY_TypePointer, cy.PointerT);
    try t.eq(c.CY_TypeFile, cy.FileT);
    try t.eq(c.CY_TypeDir, cy.DirT);
    try t.eq(c.CY_TypeDirIter, cy.DirIteratorT);
    try t.eq(c.CY_TypeType, cy.TypeSymbolT);
}

export fn cyValueGetTypeId(val: Value) c.CyTypeId {
    return val.getTypeId();
}

test "cyValueGetType()" {
    try t.eq(c.cyValueGetTypeId(c.cyValueNumber(123)), cy.NumberT);
}

export fn cyListLen(list: Value) usize {
    return list.asHeapObject().list.list.len;
}

export fn cyListCap(list: Value) usize {
    return list.asHeapObject().list.list.cap;
}

export fn cyListGet(vm: *cy.UserVM, list: Value, idx: usize) Value {
    const res = list.asHeapObject().list.list.ptr[idx];
    vm.retain(res);
    return res;
}

export fn cyListSet(vm: *cy.UserVM, list: Value, idx: usize, val: Value) void {
    vm.retain(val);
    vm.release(list.asHeapObject().list.list.ptr[idx]);
    list.asHeapObject().list.list.ptr[idx] = val;
}

export fn cyListInsert(vm: *cy.UserVM, list: Value, idx: usize, val: Value) void {
    vm.retain(val);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.asHeapObject().list.list);
    inner.growTotalCapacity(vm.allocator(), inner.len + 1) catch stdx.fatal();
    inner.insertAssumeCapacity(idx, val);
}

export fn cyListAppend(vm: *cy.UserVM, list: Value, val: Value) void {
    vm.retain(val);
    return list.asHeapObject().list.append(vm.allocator(), val);
}

test "List ops." {
    const vm = c.cyVmCreate();
    defer c.cyVmDestroy(vm);

    var list: c.CyValue = undefined;
    _ = c.cyVmEval(vm, initCStr("[1, 2, 3]"), &list);

    // Initial cap.
    try t.eq(c.cyListLen(list), 3);
    try t.eq(c.cyListCap(list), 3);

    // Append.
    c.cyListAppend(vm, list, c.cyValueNumber(4));
    var res = c.cyListGet(vm, list, 3);
    try t.eq(c.cyValueAsNumber(res), 4);
    try t.eq(c.cyListLen(list), 4);
    try t.eq(c.cyListCap(list), 12);

    // Get.
    res = c.cyListGet(vm, list, 1);
    try t.eq(c.cyValueAsNumber(res), 2);

    // Set.
    c.cyListSet(vm, list, 1, c.cyValueNumber(100));
    res = c.cyListGet(vm, list, 1);
    try t.eq(c.cyValueAsNumber(res), 100);

    // Insert.
    c.cyListInsert(vm, list, 0, c.cyValueNumber(123));
    res = c.cyListGet(vm, list, 0);
    try t.eq(c.cyValueAsNumber(res), 123);
    try t.eq(c.cyListLen(list), 5);
}

export fn cyGetFullVersion() c.CStr {
    return initCStr(build_options.full_version.ptr[0..build_options.full_version.len :0]);
}

test "cyGetFullVersion()" {
    const str = c.cyGetFullVersion();
    try t.eqStr(str.charz[0..str.len], build_options.full_version);
}

export fn cyGetVersion() c.CStr {
    return initCStr(build_options.version.ptr[0..build_options.version.len :0]);
}

test "cyGetVersion()" {
    const str = c.cyGetVersion();
    try t.eqStr(str.charz[0..str.len], build_options.version);
}

export fn cyGetBuild() c.CStr {
    return initCStr(build_options.build.ptr[0..build_options.build.len :0]);
}

test "cyGetBuild()" {
    const str = c.cyGetBuild();
    try t.eqStr(str.charz[0..str.len], build_options.build);
}

export fn cyGetCommit() c.CStr {
    return initCStr(build_options.commit.ptr[0..build_options.commit.len :0]);
}

test "cyGetCommit()" {
    const str = c.cyGetCommit();
    try t.eqStr(str.charz[0..str.len], build_options.commit);
}

fn initCStr(str: [:0]const u8) c.CStr {
    return c.CStr{
        .charz = str.ptr,
        .len = str.len,
    };
}