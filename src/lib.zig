const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = cy.fatal;
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const rt = cy.rt;
const Value = cy.Value;
const t = stdx.testing;
const log = cy.log.scoped(.lib);
const bt = cy.types.BuiltinTypes;
const c = @import("clib.zig");

export fn csCreate() *cy.UserVM {
    const alloc = cy.heap.getAllocator();
    const vm = alloc.create(cy.VM) catch fatal();
    vm.init(alloc) catch fatal();
    return @ptrCast(vm);
}

export fn csDeinit(uvm: *cy.UserVM) void {
    // Reset VM, don't free heap pages to allow object counting.
    uvm.internal().deinit(true);
}

export fn csDestroy(uvm: *cy.UserVM) void {
    uvm.deinit();
    const alloc = cy.heap.getAllocator();
    const vm = uvm.internal();
    alloc.destroy(vm);
}

export fn csGetGlobalRC(vm: *cy.UserVM) usize {
    return cy.arc.getGlobalRC(vm.internal());
}

export fn csCountObjects(vm: *cy.UserVM) usize {
    return cy.arc.countObjects(vm.internal());
}

/// This is useful when calling into wasm to allocate some memory.
export fn csAlloc(vm: *cy.UserVM, size: usize) [*]const align(8) u8 {
    const slice = vm.allocator().alignedAlloc(u8, 8, size) catch fatal();
    return slice.ptr;
}

export fn csFree(vm: *cy.UserVM, ptr: [*]const align(8) u8, size: usize) void {
    vm.allocator().free(ptr[0..size]);
}

export fn csFreeStr(vm: *cy.UserVM, str: cy.Str) void {
    vm.allocator().free(str.slice());
}

export fn csEval(vm: *cy.UserVM, src: cy.Str, outVal: *cy.Value) c.CsResultCode {
    outVal.* = vm.eval("main", src.slice(), .{
        .singleRun = false,
    }) catch |err| {
        switch (err) {
            error.TokenError => {
                return c.CS_ERROR_TOKEN;
            },
            error.ParseError => {
                return c.CS_ERROR_PARSE;
            },
            error.CompileError => {
                return c.CS_ERROR_COMPILE;
            },
            error.Panic => {
                return c.CS_ERROR_PANIC;
            },
            else => {
                return c.CS_ERROR_UNKNOWN;
            },
        }
    };
    return c.CS_SUCCESS;
}

export fn csValidate(vm: *cy.UserVM, src: cy.Str) c.CsResultCode {
    const res = vm.internal().validate("main", src.slice(), .{}) catch |err| {
        log.debug("validate error: {}", .{err});
        return c.CS_ERROR_UNKNOWN;
    };
    if (res.err) |err| {
        switch (err) {
            .tokenize => {
                return c.CS_ERROR_TOKEN;
            },
            .parse => {
                return c.CS_ERROR_PARSE;
            },
            .compile => {
                return c.CS_ERROR_COMPILE;
            },
        }
    }
    return c.CS_SUCCESS;
}

test "csValidate()" {
    const vm = c.csCreate();
    defer c.csDestroy(vm);

    cy.silentError = true;
    defer cy.silentError = false;

    var res = c.csValidate(vm, initStrSlice("1 + 2"));
    try t.eq(res, c.CS_SUCCESS);

    res = c.csValidate(vm, initStrSlice("1 +"));
    try t.eq(res, c.CS_ERROR_PARSE);
}

var tempBuf: [1024]u8 align(8) = undefined;

export fn csNewLastErrorReport(vm: *cy.UserVM) cy.Str {
    const report = vm.allocLastErrorReport() catch fatal();
    return cy.Str.initSlice(report);
}

export fn csGetResolver(vm: *cy.UserVM) c.CsResolverFn {
    return @ptrCast(vm.getModuleResolver());
}

export fn csSetResolver(vm: *cy.UserVM, resolver: c.CsResolverFn) void {
    vm.setModuleResolver(@ptrCast(resolver));
}

export fn csGetModuleLoader(vm: *cy.UserVM) c.CsModuleLoaderFn {
    return @ptrCast(vm.getModuleLoader());
}

export fn csSetModuleLoader(vm: *cy.UserVM, loader: c.CsModuleLoaderFn) void {
    vm.setModuleLoader(@ptrCast(loader));
}

export fn csGetPrint(vm: *cy.UserVM) c.CsPrintFn {
    return @ptrCast(vm.getPrint());
}

export fn csSetPrint(vm: *cy.UserVM, print: c.CsPrintFn) void {
    vm.setPrint(@ptrCast(print));
}

export fn csPerformGC(vm: *cy.UserVM) c.CsGCResult {
    const res = cy.arc.performGC(vm.internal()) catch cy.fatal();
    return .{
        .numCycFreed = res.numCycFreed,
        .numObjFreed = res.numObjFreed,
    };
}

export fn csDeclareUntypedFunc(mod: cy.ApiModule, name: [*:0]const u8, numParams: u32, funcPtr: c.CsFuncFn) void {
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = mod.sym.cast(.chunk).getMod().chunk;
    const funcSigId = chunk.sema.ensureUntypedFuncSig(numParams) catch fatal();
    if (mod.sym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const func = chunk.declareHostFunc(mod.sym, symName, funcSigId, cy.NullId, @ptrCast(funcPtr), false) catch cy.fatal();
    if (nameOwned) {
        const sym = func.sym.?;
        sym.head.setNameOwned(true);
    }
}

export fn csDeclareFunc(mod: cy.ApiModule, name: [*:0]const u8, params: [*]const cy.TypeId, numParams: u32, retType: cy.TypeId, funcPtr: c.CsFuncFn) void {
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = mod.sym.cast(.chunk).getMod().chunk;
    const funcSigId = chunk.sema.ensureFuncSig(params[0..numParams], retType) catch cy.fatal();
    if (mod.sym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const func = chunk.declareHostFunc(mod.sym, symName, funcSigId, cy.NullId, @ptrCast(funcPtr), false) catch cy.fatal();
    if (nameOwned) {
        const sym = func.sym.?;
        sym.head.setNameOwned(true);
    }
}

export fn csDeclareVar(mod: cy.ApiModule, name: [*:0]const u8, typeId: cy.TypeId, val: c.CsValue) void {
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = mod.sym.cast(.chunk).getMod().chunk;
    if (mod.sym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const sym = chunk.declareHostVar(mod.sym, symName, cy.NullId, typeId, @bitCast(val)) catch cy.fatal();
    if (nameOwned) {
        sym.head.setNameOwned(true);
    }
}

export fn csRelease(vm: *cy.UserVM, val: Value) void {
    vm.release(val);
}

export fn csRetain(vm: *cy.UserVM, val: Value) void {
    vm.retain(val);
}

export fn csGetUserData(vm: *cy.UserVM) ?*anyopaque {
    return vm.getUserData();
}

export fn csSetUserData(vm: *cy.UserVM, userData: ?*anyopaque) void {
    vm.setUserData(userData);
}

export fn csNone() Value {
    return Value.None;
}

export fn csTrue() Value {
    return Value.True;
}

export fn csFalse() Value {
    return Value.False;
}

export fn csBool(b: bool) Value {
    return Value.initBool(b);
}

export fn csFloat(n: f64) Value {
    return Value.initF64(n);
}

export fn csInteger(n: i64) Value {
    return Value.initInt(@intCast(n));
}

export fn csInteger32(n: i32) Value {
    return Value.initInt(n);
}

export fn csHostObject(ptr: *anyopaque) Value {
    return Value.initHostPtr(ptr);
}

export fn csVmObject(ptr: *anyopaque) Value {
    return Value.initPtr(ptr);
}

export fn csNewString(vm: *cy.UserVM, cstr: cy.Str) Value {
    return vm.retainOrAllocString(cstr.slice()) catch fatal();
}

export fn csNewAstring(vm: *cy.VM, cstr: cy.Str) Value {
    return vm.retainOrAllocAstring(cstr.slice()) catch fatal();
}

export fn csNewUstring(vm: *cy.UserVM, cstr: cy.Str, charLen: u32) Value {
    return vm.retainOrAllocUstring(cstr.slice(), charLen) catch fatal();
}

export fn csNewTuple(vm: *cy.UserVM, ptr: [*]const Value, len: usize) Value {
    const elems = ptr[0..len];
    for (elems) |elem| {
        cy.arc.retain(vm.internal(), elem);
    }
    return cy.heap.allocTuple(vm.internal(), elems) catch fatal();
}

export fn csNewEmptyList(vm: *cy.UserVM) Value {
    return vm.allocEmptyList() catch fatal();
}

export fn csNewList(vm: *cy.UserVM, ptr: [*]const Value, len: usize) Value {
    const elems = ptr[0..len];
    for (elems) |elem| {
        cy.arc.retain(vm.internal(), elem);
    }
    return vm.allocList(elems) catch fatal();
}

export fn csNewEmptyMap(vm: *cy.UserVM) Value {
    return vm.allocEmptyMap() catch fatal();
}

export fn csNewUntypedFunc(vm: *cy.VM, numParams: u32, func: c.CsFuncFn) Value {
    const funcSigId = vm.sema.ensureUntypedFuncSig(numParams) catch fatal();
    return vm.allocHostFunc(@ptrCast(func), numParams, funcSigId, null) catch fatal();
}

export fn csNewFunc(vm: *cy.VM, params: [*]const cy.TypeId, numParams: u32, retType: cy.TypeId, func: c.CsFuncFn) Value {
    const funcSigId = vm.sema.ensureFuncSig(params[0..numParams], retType) catch fatal();
    return vm.allocHostFunc(@ptrCast(func), numParams, funcSigId, null) catch fatal();
}

test "csNewFunc()" {
    const vm = c.csCreate();
    defer c.csDestroy(vm);

    const val = c.csNewFunc(vm, &[_]cy.TypeId{}, 0, bt.Dynamic, @ptrFromInt(8));
    try t.eq(c.csGetTypeId(val), bt.HostFunc);
}

export fn csNewHostObject(vm: *cy.UserVM, typeId: cy.TypeId, size: usize) Value {
    const ptr = cy.heap.allocHostCycObject(vm.internal(), typeId, size) catch cy.fatal();
    return Value.initHostCycPtr(ptr);
}

export fn csNewHostObjectPtr(vm: *cy.UserVM, typeId: cy.TypeId, size: usize) *anyopaque {
    return cy.heap.allocHostCycObject(vm.internal(), typeId, size) catch cy.fatal();
}

export fn csNewVmObject(vm: *cy.VM, typeId: cy.TypeId, argsPtr: [*]const Value, numArgs: usize) Value {
    const entry = &vm.types[typeId];
    std.debug.assert(entry.sym.type == .object);

    std.debug.assert(numArgs == entry.data.numFields);
    const args = argsPtr[0..numArgs];
    for (args) |arg| {
        cy.arc.retain(vm, arg);
    }
    if (numArgs <= 4) {
        return cy.heap.allocObjectSmall(vm, typeId, args) catch cy.fatal();
    } else {
        return cy.heap.allocObject(vm, typeId, args) catch cy.fatal();
    }
}

export fn csSymbol(vm: *cy.UserVM, str: cy.Str) Value {
    const id = vm.internal().ensureSymbolExt(str.slice(), true) catch fatal();
    return Value.initSymbol(@intCast(id));
}

export fn csNewPointer(vm: *cy.UserVM, ptr: ?*anyopaque) Value {
    return cy.heap.allocPointer(vm.internal(), ptr) catch fatal();
}

test "csNewPointer()" {
    const vm = c.csCreate();
    defer c.csDestroy(vm);

    const val = c.csNewPointer(vm, @ptrFromInt(123));
    try t.eq(c.csGetTypeId(val), bt.Pointer);

    const obj = (Value{.val = val}).asHeapObject();
    try t.eq(@intFromPtr(obj.pointer.ptr), 123);
}

export fn csAsFloat(val: Value) f64 {
    return val.asF64();
}

test "csAsFloat()" {
    try t.eq(c.csAsFloat(c.csFloat(123.0)), 123);
}

export fn csToBool(val: Value) bool {
    return val.toBool();
}

test "csToBool()" {
    try t.eq(c.csToBool(c.csFloat(123.0)), true);
    try t.eq(c.csToBool(c.csFloat(0)), true);
    try t.eq(c.csToBool(c.csTrue()), true);
    try t.eq(c.csToBool(c.csNone()), false);
    try t.eq(c.csToBool(c.csFalse()), false);
}

export fn csAsBool(val: Value) bool {
    return val.asBool();
}

test "csAsBool()" {
    try t.eq(c.csAsBool(c.csTrue()), true);
    try t.eq(c.csAsBool(c.csFalse()), false);
}

export fn csAsInteger(val: Value) i64 {
    return val.asInteger();
}

test "csAsInteger()" {
    try t.eq(c.csAsInteger(c.csInteger(123)), 123);
}

export fn csAsSymbolId(val: Value) u32 {
    return val.asSymbolId();
}

test "csAsSymbolId()" {
    const vm = c.csCreate();
    defer c.csDestroy(vm);

    var str = initStrSlice("foo");
    var val = c.csSymbol(vm, str);
    try t.eq(c.csAsSymbolId(val), 0);

    str = initStrSlice("bar");
    val = c.csSymbol(vm, str);
    try t.eq(c.csAsSymbolId(val), 1);

    str = initStrSlice("foo");
    val = c.csSymbol(vm, str);
    try t.eq(c.csAsSymbolId(val), 0);
}

export fn csToTempString(vm: *cy.UserVM, val: Value) cy.Str {
    const str = vm.valueToTempString(val);
    return cy.Str.initSlice(str);
}

export fn csToTempByteArray(vm: *cy.UserVM, val: Value) cy.Str {
    const str = vm.valueToTempByteArray(val);
    return cy.Str.initSlice(str);
}

test "Constants." {
    try t.eq(c.CS_TYPE_NONE, bt.None);
    try t.eq(c.CS_TYPE_BOOLEAN, bt.Boolean);
    try t.eq(c.CS_TYPE_ERROR, bt.Error);
    try t.eq(c.CS_TYPE_SYMBOL, bt.Symbol);
    try t.eq(c.CS_TYPE_INTEGER, bt.Integer);
    try t.eq(c.CS_TYPE_FLOAT, bt.Float);
    try t.eq(c.CS_TYPE_LIST, bt.List);
    try t.eq(c.CS_TYPE_LISTITER, bt.ListIter);
    try t.eq(c.CS_TYPE_MAP, bt.Map);
    try t.eq(c.CS_TYPE_MAPITER, bt.MapIter);
    try t.eq(c.CS_TYPE_CLOSURE, bt.Closure);
    try t.eq(c.CS_TYPE_LAMBDA, bt.Lambda);
    try t.eq(c.CS_TYPE_STRING, bt.String);
    try t.eq(c.CS_TYPE_ARRAY, bt.Array);
    try t.eq(c.CS_TYPE_FIBER, bt.Fiber);
    try t.eq(c.CS_TYPE_BOX, bt.Box);
    try t.eq(c.CS_TYPE_NATIVEFUNC1, bt.HostFunc);
    try t.eq(c.CS_TYPE_TCCSTATE, bt.TccState);
    try t.eq(c.CS_TYPE_POINTER, bt.Pointer);
    try t.eq(c.CS_TYPE_TUPLE, bt.Tuple);
    try t.eq(c.CS_TYPE_METATYPE, bt.MetaType);
}

export fn csAsHostObject(val: Value) *anyopaque {
    return val.asHostObject(*anyopaque);
}

export fn csGetTypeId(val: Value) c.CsTypeId {
    return val.getTypeId();
}

test "csGetTypeId()" {
    try t.eq(c.csGetTypeId(c.csFloat(123)), bt.Float);
}

export fn csListLen(list: Value) usize {
    return list.asHeapObject().list.list.len;
}

export fn csListCap(list: Value) usize {
    return list.asHeapObject().list.list.cap;
}

export fn csListGet(vm: *cy.UserVM, list: Value, idx: usize) Value {
    const res = list.asHeapObject().list.list.ptr[idx];
    vm.retain(res);
    return res;
}

export fn csListSet(vm: *cy.UserVM, list: Value, idx: usize, val: Value) void {
    vm.retain(val);
    vm.release(list.asHeapObject().list.list.ptr[idx]);
    list.asHeapObject().list.list.ptr[idx] = val;
}

export fn csListInsert(vm: *cy.UserVM, list: Value, idx: usize, val: Value) void {
    vm.retain(val);
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.asHeapObject().list.list);
    inner.growTotalCapacity(vm.allocator(), inner.len + 1) catch cy.fatal();
    inner.insertAssumeCapacity(idx, val);
}

export fn csListAppend(vm: *cy.UserVM, list: Value, val: Value) void {
    vm.retain(val);
    return list.asHeapObject().list.append(vm.allocator(), val);
}

test "List ops." {
    const vm = c.csCreate();
    defer c.csDestroy(vm);

    var list: c.CsValue = undefined;
    _ = c.csEval(vm, initStrSlice("[1, 2, 3]"), &list);

    // Initial cap.
    try t.eq(c.csListLen(list), 3);
    try t.eq(c.csListCap(list), 3);

    // Append.
    c.csListAppend(vm, list, c.csFloat(4));
    var res = c.csListGet(vm, list, 3);
    try t.eq(c.csAsFloat(res), 4);
    try t.eq(c.csListLen(list), 4);
    try t.eq(c.csListCap(list), 12);

    // Get.
    res = c.csListGet(vm, list, 1);
    try t.eq(c.csAsInteger(res), 2);

    // Set.
    c.csListSet(vm, list, 1, c.csFloat(100));
    res = c.csListGet(vm, list, 1);
    try t.eq(c.csAsFloat(res), 100);

    // Insert.
    c.csListInsert(vm, list, 0, c.csFloat(123));
    res = c.csListGet(vm, list, 0);
    try t.eq(c.csAsFloat(res), 123);
    try t.eq(c.csListLen(list), 5);
}

export fn csGetFullVersion() cy.Str {
    return cy.Str.initSlice(build_options.full_version.ptr[0..build_options.full_version.len]);
}

test "csGetFullVersion()" {
    const str = c.csGetFullVersion();
    try t.eqStr(str.buf[0..str.len], build_options.full_version);
}

export fn csGetVersion() cy.Str {
    return cy.Str.initSlice(build_options.version.ptr[0..build_options.version.len]);
}

test "csGetVersion()" {
    const str = c.csGetVersion();
    try t.eqStr(str.buf[0..str.len], build_options.version);
}

export fn csGetBuild() cy.Str {
    return cy.Str.initSlice(build_options.build.ptr[0..build_options.build.len]);
}

test "csGetBuild()" {
    const str = c.csGetBuild();
    try t.eqStr(str.buf[0..str.len], build_options.build);
}

export fn csGetCommit() cy.Str {
    return cy.Str.initSlice(build_options.commit.ptr[0..build_options.commit.len]);
}

test "csGetCommit()" {
    const str = c.csGetCommit();
    try t.eqStr(str.buf[0..str.len], build_options.commit);
}

fn initStrSlice(s: []const u8) c.CsStr {
    return .{
        .buf = s.ptr,
        .len = s.len,
    };
}