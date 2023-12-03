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

export fn csDeinit(vm: *cy.VM) void {
    // Reset VM, don't free heap pages to allow object counting.
    vm.deinit(true);
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

export fn csFree(vm: *cy.VM, ptr: [*]const align(8) u8, size: usize) void {
    vm.alloc.free(ptr[0..size]);
}

export fn csFreeStr(vm: *cy.VM, str: c.Str) void {
    vm.alloc.free(c.strSlice(str));
}

export fn csFreeStrZ(vm: *cy.VM, str: [*:0]const u8) void {
    vm.alloc.free(std.mem.sliceTo(str, 0));
}

export fn csEval(vm: *cy.VM, src: c.Str, outVal: *cy.Value) c.ResultCode {
    outVal.* = vm.eval("main", c.strSlice(src), .{
        .singleRun = false,
    }) catch |err| {
        switch (err) {
            error.TokenError => {
                return c.ErrorToken;
            },
            error.ParseError => {
                return c.ErrorParse;
            },
            error.CompileError => {
                return c.ErrorCompile;
            },
            error.Panic => {
                return c.ErrorPanic;
            },
            else => {
                return c.ErrorUnknown;
            },
        }
    };
    return c.Success;
}

export fn csValidate(vm: *cy.UserVM, src: c.Str) c.ResultCode {
    const res = vm.internal().validate("main", c.strSlice(src), .{}) catch |err| {
        log.debug("validate error: {}", .{err});
        return c.ErrorUnknown;
    };
    if (res.err) |err| {
        switch (err) {
            .tokenize => {
                return c.ErrorToken;
            },
            .parse => {
                return c.ErrorParse;
            },
            .compile => {
                return c.ErrorCompile;
            },
        }
    }
    return c.Success;
}

test "csValidate()" {
    const vm = c.create();
    defer c.destroy(vm);

    cy.silentError = true;
    defer cy.silentError = false;

    var res = c.validate(vm, c.initStr("1 + 2"));
    try t.eq(res, c.Success);

    res = c.validate(vm, c.initStr("1 +"));
    try t.eq(res, c.ErrorParse);
}

var tempBuf: [1024]u8 align(8) = undefined;

export fn csNewLastErrorReport(vm: *cy.VM) [*:0]const u8 {
    return vm.allocLastErrorReport() catch fatal();
}

export fn csGetResolver(vm: *cy.VM) c.ResolverFn {
    return vm.compiler.moduleResolver;
}

export fn csSetResolver(vm: *cy.VM, resolver: c.ResolverFn) void {
    vm.compiler.moduleResolver = resolver;
}

export fn csGetModuleLoader(vm: *cy.VM) c.ModuleLoaderFn {
    return vm.compiler.moduleLoader;
}

export fn csSetModuleLoader(vm: *cy.VM, loader: c.ModuleLoaderFn) void {
    vm.compiler.moduleLoader = loader;
}

export fn csGetPrint(vm: *cy.VM) c.PrintFn {
    return vm.print;
}

export fn csSetPrint(vm: *cy.VM, print: c.PrintFn) void {
    vm.print = print;
}

export fn csPerformGC(vm: *cy.UserVM) c.GCResult {
    const res = cy.arc.performGC(vm.internal()) catch cy.fatal();
    return .{
        .numCycFreed = res.numCycFreed,
        .numObjFreed = res.numObjFreed,
    };
}

export fn csDeclareUntypedFunc(mod: c.ApiModule, name: [*:0]const u8, numParams: u32, funcPtr: c.FuncFn) void {
    const modSym: *cy.Sym = @ptrCast(@alignCast(mod.sym));
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = modSym.cast(.chunk).getMod().chunk;
    const funcSigId = chunk.sema.ensureUntypedFuncSig(numParams) catch fatal();
    if (modSym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const func = chunk.declareHostFunc(modSym, symName, funcSigId, cy.NullId, @ptrCast(funcPtr), false) catch cy.fatal();
    if (nameOwned) {
        const sym = func.sym.?;
        sym.head.setNameOwned(true);
    }
}

export fn csDeclareFunc(mod: c.ApiModule, name: [*:0]const u8, params: [*]const cy.TypeId, numParams: u32, retType: cy.TypeId, funcPtr: c.FuncFn) void {
    const modSym: *cy.Sym = @ptrCast(@alignCast(mod.sym));
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = modSym.cast(.chunk).getMod().chunk;
    const funcSigId = chunk.sema.ensureFuncSig(params[0..numParams], retType) catch cy.fatal();
    if (modSym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const func = chunk.declareHostFunc(modSym, symName, funcSigId, cy.NullId, @ptrCast(funcPtr), false) catch cy.fatal();
    if (nameOwned) {
        const sym = func.sym.?;
        sym.head.setNameOwned(true);
    }
}

export fn csDeclareVar(mod: c.ApiModule, name: [*:0]const u8, typeId: cy.TypeId, val: c.Value) void {
    const modSym: *cy.Sym = @ptrCast(@alignCast(mod.sym));
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = modSym.cast(.chunk).getMod().chunk;
    if (modSym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const sym = chunk.declareHostVar(modSym, symName, cy.NullId, typeId, @bitCast(val)) catch cy.fatal();
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

export fn csGetUserData(vm: *cy.VM) ?*anyopaque {
    return vm.userData;
}

export fn csSetUserData(vm: *cy.VM, userData: ?*anyopaque) void {
    vm.userData = userData;
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

export fn csNewString(vm: *cy.VM, cstr: c.Str) Value {
    return vm.allocStringInternOrArray(c.strSlice(cstr)) catch fatal();
}

export fn csNewAstring(vm: *cy.VM, cstr: c.Str) Value {
    return vm.retainOrAllocAstring(c.strSlice(cstr)) catch fatal();
}

export fn csNewUstring(vm: *cy.UserVM, cstr: c.Str, charLen: u32) Value {
    return vm.retainOrAllocUstring(c.strSlice(cstr), charLen) catch fatal();
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

export fn csNewUntypedFunc(vm: *cy.VM, numParams: u32, func: c.FuncFn) Value {
    const funcSigId = vm.sema.ensureUntypedFuncSig(numParams) catch fatal();
    return vm.allocHostFunc(@ptrCast(func), numParams, funcSigId, null) catch fatal();
}

export fn csNewFunc(vm: *cy.VM, params: [*]const cy.TypeId, numParams: u32, retType: cy.TypeId, func: c.FuncFn) Value {
    const funcSigId = vm.sema.ensureFuncSig(params[0..numParams], retType) catch fatal();
    return vm.allocHostFunc(@ptrCast(func), numParams, funcSigId, null) catch fatal();
}

test "csNewFunc()" {
    const vm = c.create();
    defer c.destroy(vm);

    const val = c.newFunc(vm, &[_]cy.TypeId{}, 0, bt.Dynamic, @ptrFromInt(8));
    try t.eq(c.getTypeId(val), bt.HostFunc);
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

export fn csSymbol(vm: *cy.UserVM, str: c.Str) Value {
    const id = vm.internal().ensureSymbolExt(c.strSlice(str), true) catch fatal();
    return Value.initSymbol(@intCast(id));
}

export fn csNewPointer(vm: *cy.UserVM, ptr: ?*anyopaque) Value {
    return cy.heap.allocPointer(vm.internal(), ptr) catch fatal();
}

test "csNewPointer()" {
    const vm = c.create();
    defer c.destroy(vm);

    const val = c.newPointer(vm, @ptrFromInt(123));
    try t.eq(c.getTypeId(val), bt.Pointer);

    const obj = (Value{.val = val}).asHeapObject();
    try t.eq(@intFromPtr(obj.pointer.ptr), 123);
}

export fn csAsFloat(val: Value) f64 {
    return val.asF64();
}

test "csAsFloat()" {
    try t.eq(c.asFloat(c.float(123.0)), 123);
}

export fn csToBool(val: Value) bool {
    return val.toBool();
}

test "csToBool()" {
    try t.eq(c.toBool(c.float(123.0)), true);
    try t.eq(c.toBool(c.float(0)), true);
    try t.eq(c.toBool(c.csTrue()), true);
    try t.eq(c.toBool(c.none()), false);
    try t.eq(c.toBool(c.csFalse()), false);
}

export fn csAsBool(val: Value) bool {
    return val.asBool();
}

test "csAsBool()" {
    try t.eq(c.asBool(c.csTrue()), true);
    try t.eq(c.asBool(c.csFalse()), false);
}

export fn csAsInteger(val: Value) i64 {
    return val.asInteger();
}

test "csAsInteger()" {
    try t.eq(c.asInteger(c.integer(123)), 123);
}

export fn csAsSymbolId(val: Value) u32 {
    return val.asSymbolId();
}

test "csAsSymbolId()" {
    const vm = c.create();
    defer c.destroy(vm);

    var str = c.initStr("foo");
    var val = c.symbol(vm, str);
    try t.eq(c.asSymbolId(val), 0);

    str = c.initStr("bar");
    val = c.symbol(vm, str);
    try t.eq(c.asSymbolId(val), 1);

    str = c.initStr("foo");
    val = c.symbol(vm, str);
    try t.eq(c.asSymbolId(val), 0);
}

export fn csToTempString(vm: *cy.VM, val: Value) c.Str {
    const str = vm.getOrBufPrintValueStr(&cy.tempBuf, val) catch cy.fatal();
    return c.initStr(str);
}

export fn csToTempByteArray(vm: *cy.VM, val: Value) c.Str {
    const str = vm.getOrBufPrintValueRawStr(&cy.tempBuf, val) catch cy.fatal();
    return c.initStr(str);
}

test "Constants." {
    try t.eq(c.TypeNone, bt.None);
    try t.eq(c.TypeBoolean, bt.Boolean);
    try t.eq(c.TypeError, bt.Error);
    try t.eq(c.TypeSymbol, bt.Symbol);
    try t.eq(c.TypeInteger, bt.Integer);
    try t.eq(c.TypeFloat, bt.Float);
    try t.eq(c.TypeList, bt.List);
    try t.eq(c.TypeListIter, bt.ListIter);
    try t.eq(c.TypeMap, bt.Map);
    try t.eq(c.TypeMapIter, bt.MapIter);
    try t.eq(c.TypeClosure, bt.Closure);
    try t.eq(c.TypeLambda, bt.Lambda);
    try t.eq(c.TypeString, bt.String);
    try t.eq(c.TypeArray, bt.Array);
    try t.eq(c.TypeFiber, bt.Fiber);
    try t.eq(c.TypeBox, bt.Box);
    try t.eq(c.TypeHostFunc, bt.HostFunc);
    try t.eq(c.TypeTccState, bt.TccState);
    try t.eq(c.TypePointer, bt.Pointer);
    try t.eq(c.TypeTuple, bt.Tuple);
    try t.eq(c.TypeMetaType, bt.MetaType);
}

export fn csAsHostObject(val: Value) *anyopaque {
    return val.castHostObject(*anyopaque);
}

export fn csGetTypeId(val: Value) c.TypeId {
    return val.getTypeId();
}

test "csGetTypeId()" {
    try t.eq(c.getTypeId(c.float(123)), bt.Float);
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

export fn csListAppend(vm: *cy.VM, list: Value, val: Value) void {
    vm.retain(val);
    return list.asHeapObject().list.append(vm.alloc, val) catch cy.fatal();
}

test "List ops." {
    const vm = c.create();
    defer c.destroy(vm);

    var list: c.Value = undefined;
    _ = c.eval(vm, c.initStr("[1, 2, 3]"), &list);

    // Initial cap.
    try t.eq(c.listLen(list), 3);
    try t.eq(c.listCap(list), 3);

    // Append.
    c.listAppend(vm, list, c.float(4));
    var res = c.listGet(vm, list, 3);
    try t.eq(c.asFloat(res), 4);
    try t.eq(c.listLen(list), 4);
    try t.eq(c.listCap(list), 12);

    // Get.
    res = c.listGet(vm, list, 1);
    try t.eq(c.asInteger(res), 2);

    // Set.
    c.listSet(vm, list, 1, c.float(100));
    res = c.listGet(vm, list, 1);
    try t.eq(c.asFloat(res), 100);

    // Insert.
    c.listInsert(vm, list, 0, c.float(123));
    res = c.listGet(vm, list, 0);
    try t.eq(c.asFloat(res), 123);
    try t.eq(c.listLen(list), 5);
}

export fn csGetFullVersion() c.Str {
    return c.initStr(build_options.full_version.ptr[0..build_options.full_version.len]);
}

test "csGetFullVersion()" {
    const str = c.getFullVersion();
    try t.eqStr(str.buf[0..str.len], build_options.full_version);
}

export fn csGetVersion() c.Str {
    return c.initStr(build_options.version.ptr[0..build_options.version.len]);
}

test "csGetVersion()" {
    const str = c.getVersion();
    try t.eqStr(str.buf[0..str.len], build_options.version);
}

export fn csGetBuild() c.Str {
    return c.initStr(build_options.build.ptr[0..build_options.build.len]);
}

test "csGetBuild()" {
    const str = c.getBuild();
    try t.eqStr(str.buf[0..str.len], build_options.build);
}

export fn csGetCommit() c.Str {
    return c.initStr(build_options.commit.ptr[0..build_options.commit.len]);
}

test "csGetCommit()" {
    const str = c.getCommit();
    try t.eqStr(str.buf[0..str.len], build_options.commit);
}