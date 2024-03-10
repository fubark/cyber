const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = cy.fatal;
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const Value = cy.Value;
const t = stdx.testing;
const log = cy.log.scoped(.lib);
const bt = cy.types.BuiltinTypes;
const c = @import("capi.zig");

const cli = @import("cli.zig");

comptime {
    if (build_options.cli or builtin.os.tag == .wasi) {
        std.testing.refAllDecls(cli);
    }
}

export var csVerbose = false;
export var csSilent = false;

export fn csCreate() *cy.VM {
    const alloc = cy.heap.getAllocator();
    const vm = alloc.create(cy.VM) catch fatal();
    vm.init(alloc) catch fatal();
    return @ptrCast(vm);
}

export fn csDeinit(vm: *cy.VM) void {
    vm.deinitRtObjects();
    vm.compiler.deinitModRetained();
}

export fn csDestroy(vm: *cy.VM) void {
    vm.deinit(false);
    const alloc = cy.heap.getAllocator();
    alloc.destroy(vm);
}

export fn csGetGlobalRC(vm: *cy.VM) usize {
    return cy.arc.getGlobalRC(vm);
}

export fn csCountObjects(vm: *cy.VM) usize {
    return cy.arc.countObjects(vm);
}

export fn csTraceDumpLiveObjects(vm: *cy.VM) void {
    if (cy.Trace) {
        var iter = vm.objectTraceMap.iterator();
        while (iter.next()) |it| {
            const trace = it.value_ptr.*;
            if (trace.freePc == cy.NullId) {
                cy.arc.dumpObjectAllocTrace(vm, it.key_ptr.*, trace.allocPc) catch fatal();
            }
        }
    }
}

/// This is useful when calling into wasm to allocate some memory.
export fn csAlloc(vm: *cy.VM, size: usize) c.Slice {
    const slice = vm.alloc.alignedAlloc(u8, 8, size) catch fatal();
    return .{ .buf = slice.ptr, .len = size };
}

export fn csFree(vm: *cy.VM, slice: c.Slice) void {
    vm.alloc.free(slice.buf[0..slice.len]);
}

export fn csFreeStr(vm: *cy.VM, str: c.Str) void {
    vm.alloc.free(c.fromStr(str));
}

export fn csFreeStrZ(vm: *cy.VM, str: [*:0]const u8) void {
    vm.alloc.free(std.mem.sliceTo(str, 0));
}

export fn csGetAllocator(vm: *cy.VM) c.Allocator {
    return c.toAllocator(vm.alloc);
}

export fn csDefaultEvalConfig() c.EvalConfig {
    return .{
        .single_run = false,
        .file_modules = false,
        .reload = false,
        .backend = c.BackendVM,
        .gen_all_debug_syms = false,
        .spawn_exe = false, 
    };
}

export fn csEval(vm: *cy.VM, src: c.Str, outVal: *cy.Value) c.ResultCode {
    vm.deinit(true);
    const uri: []const u8 = "main";
    return csEvalExt(vm, c.toStr(uri), src, c.defaultEvalConfig(), outVal);
}

export fn csEvalExt(vm: *cy.VM, uri: c.Str, src: c.Str, config: c.EvalConfig, outVal: *cy.Value) c.ResultCode {
    var res = c.Success;
    outVal.* = vm.eval(c.fromStr(uri), c.fromStr(src), config) catch |err| b: {
        switch (err) {
            error.CompileError => {
                res = c.ErrorCompile;
            },
            error.Panic => {
                res = c.ErrorPanic;
            },
            else => {
                res = c.ErrorUnknown;
            },
        }
        break :b undefined;
    };
    vm.last_res = res;
    return res;
}

export fn csDefaultCompileConfig() c.CompileConfig {
    return .{
        .single_run = false,
        .gen_all_debug_syms = false,
        .file_modules = false,
        .backend = c.BackendVM,
        .skip_codegen = false,
        .emit_source_map = false,
        .gen_debug_func_markers = false,
    };
}

export fn csCompile(vm: *cy.VM, uri: c.Str, src: c.Str, config: c.CompileConfig) c.ResultCode {
    var res = c.Success;
    _ = vm.compile(c.fromStr(uri), c.fromStr(src), config) catch |err| {
        switch (err) {
            error.CompileError => {
                res = c.ErrorCompile;
            },
            else => {
                res = c.ErrorUnknown;
            },
        }
    };
    vm.last_res = res;
    return res;
}

export fn csValidate(vm: *cy.VM, src: c.Str) c.ResultCode {
    var res = c.Success;
    _ = vm.validate("main", c.fromStr(src), .{
        .file_modules = false,
    }) catch |err| {
        switch (err) {
            error.CompileError => {
                res = c.ErrorCompile;
            },
            else => {
                res = c.ErrorUnknown;
            },
        }
    };
    vm.last_res = res;
    return res;
}

test "csValidate()" {
    const vm = c.create();
    defer c.destroy(vm);

    c.setSilent(true);
    defer c.setSilent(false);

    var res = c.validate(vm, c.toStr("1 + 2"));
    try t.eq(res, c.Success);

    res = c.validate(vm, c.toStr("1 +"));
    try t.eq(res, c.ErrorCompile);
}

export fn csResultName(code: c.ResultCode) c.Str {
    switch (code) {
        c.Success => return c.toStr("Success"),
        c.ErrorCompile => return c.toStr("CompileError"),
        c.ErrorPanic => return c.toStr("PanicError"),
        c.ErrorUnknown => return c.toStr("UnknownError"),
        else => fatal(),
    }
}

var tempBuf: [1024]u8 align(8) = undefined;

export fn csNewLastErrorSummary(vm: *cy.VM) c.Str {
    if (vm.last_res == c.ErrorCompile) {
        return csNewErrorReportSummary(vm);
    } else if (vm.last_res == c.ErrorPanic) {
        return csNewPanicSummary(vm);
    } else {
        return c.null_str;
    }
}

export fn csNewErrorReportSummary(vm: *cy.VM) c.Str {
    if (vm.compiler.reports.items.len == 0) {
        cy.panic("No report.");
    }
    const str = cy.debug.allocReportSummary(vm.compiler, vm.compiler.reports.items[0]) catch fatal();
    return c.toStr(str);
}

export fn csNewPanicSummary(vm: *const cy.VM) c.Str {
    if (vm.config.backend == c.BackendVM) {
        const summary = cy.debug.allocLastUserPanicError(vm) catch fatal();
        return c.toStr(summary);
    } else {
        const dupe = vm.alloc.dupe(u8, vm.lastExeError) catch fatal();
        return c.toStr(dupe);
    }
}

export fn csReportApiError(vm: *const cy.VM, msg: c.Str) void {
    vm.setApiError(c.fromStr(msg)) catch fatal();
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

export fn csGetPrinter(vm: *cy.VM) c.PrintFn {
    return vm.print;
}

export fn csSetPrinter(vm: *cy.VM, print: c.PrintFn) void {
    vm.print = print;
}

export fn csGetErrorPrinter(vm: *cy.VM) c.PrintErrorFn {
    return vm.print_err;
}

export fn csSetErrorPrinter(vm: *cy.VM, print: c.PrintErrorFn) void {
    vm.print_err = print;
}

pub export var csLog: c.LogFn = cy.log.defaultLog;

export fn csPerformGC(vm: *cy.VM) c.GCResult {
    const res = cy.arc.performGC(vm) catch cy.fatal();
    return .{
        .numCycFreed = res.numCycFreed,
        .numObjFreed = res.numObjFreed,
    };
}

export fn csDeclareUntypedFunc(mod: c.Sym, name: [*:0]const u8, numParams: u32, funcPtr: c.FuncFn) void {
    const modSym = cy.Sym.fromC(mod);
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

export fn csDeclareFunc(mod: c.Sym, name: [*:0]const u8, params: [*]const cy.TypeId, numParams: u32, retType: cy.TypeId, funcPtr: c.FuncFn) void {
    const modSym = cy.Sym.fromC(mod);
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

export fn csDeclareVar(mod: c.Sym, name: [*:0]const u8, typeId: cy.TypeId, val: c.Value) void {
    const modSym = cy.Sym.fromC(mod);
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

export fn csExpandTypeTemplate(ctemplate: c.Sym, args_ptr: [*]const cy.Value, nargs: u32) c.TypeId {
    const template = cy.Sym.fromC(ctemplate).cast(.typeTemplate);
    const chunk = template.chunk();
    const args = args_ptr[0..nargs];

    // Build args types.
    const typeStart = chunk.typeStack.items.len;
    defer chunk.typeStack.items.len = typeStart;
    for (args) |arg| {
        chunk.typeStack.append(chunk.alloc, arg.getTypeId()) catch cy.fatal();
    }
    const arg_types = chunk.typeStack.items[typeStart..];
    const sym = cy.cte.expandTemplate(chunk, template, args, arg_types) catch cy.fatal();
    return sym.getStaticType().?;
}

export fn csRelease(vm: *cy.VM, val: Value) void {
    vm.release(val);
}

export fn csRetain(vm: *cy.VM, val: Value) void {
    vm.retain(val);
}

export fn csGetUserData(vm: *cy.VM) ?*anyopaque {
    return vm.userData;
}

export fn csSetUserData(vm: *cy.VM, userData: ?*anyopaque) void {
    vm.userData = userData;
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
    return vm.allocString(c.fromStr(cstr)) catch fatal();
}

export fn csNewAstring(vm: *cy.VM, cstr: c.Str) Value {
    return vm.retainOrAllocAstring(c.fromStr(cstr)) catch fatal();
}

export fn csNewUstring(vm: *cy.VM, cstr: c.Str) Value {
    return vm.retainOrAllocUstring(c.fromStr(cstr)) catch fatal();
}

export fn csNewTuple(vm: *cy.VM, ptr: [*]const Value, len: usize) Value {
    const elems = ptr[0..len];
    for (elems) |elem| {
        cy.arc.retain(vm, elem);
    }
    return cy.heap.allocTuple(vm, elems) catch fatal();
}

export fn csNewEmptyList(vm: *cy.VM) Value {
    return vm.allocEmptyList() catch fatal();
}

export fn csNewList(vm: *cy.VM, ptr: [*]const Value, len: usize) Value {
    const elems = ptr[0..len];
    for (elems) |elem| {
        cy.arc.retain(vm, elem);
    }
    return cy.heap.allocList(vm, elems) catch fatal();
}

export fn csNewEmptyMap(vm: *cy.VM) Value {
    return cy.heap.allocEmptyMap(vm) catch fatal();
}

export fn csNewUntypedFunc(vm: *cy.VM, numParams: u32, func: c.FuncFn) Value {
    const funcSigId = vm.sema.ensureUntypedFuncSig(numParams) catch fatal();
    return vm.allocHostFunc(@ptrCast(func), numParams, funcSigId, null, false) catch fatal();
}

export fn csNewFunc(vm: *cy.VM, params: [*]const cy.TypeId, numParams: u32, retType: cy.TypeId, func: c.FuncFn) Value {
    const funcSigId = vm.sema.ensureFuncSig(params[0..numParams], retType) catch fatal();
    const funcSig = vm.sema.funcSigs.items[funcSigId];
    return vm.allocHostFunc(@ptrCast(func), numParams, funcSigId, null, funcSig.reqCallTypeCheck) catch fatal();
}

test "csNewFunc()" {
    const vm = c.create();
    defer c.destroy(vm);

    const val = c.newFunc(vm, &[_]cy.TypeId{}, 0, bt.Dynamic, @ptrFromInt(8));
    try t.eq(c.getTypeId(val), bt.HostFunc);
}

export fn csNewHostObject(vm: *cy.VM, typeId: cy.TypeId, size: usize) Value {
    const ptr = cy.heap.allocHostCycObject(vm, typeId, size) catch cy.fatal();
    return Value.initHostCycPtr(ptr);
}

export fn csNewHostObjectPtr(vm: *cy.VM, typeId: cy.TypeId, size: usize) *anyopaque {
    return cy.heap.allocHostCycObject(vm, typeId, size) catch cy.fatal();
}

export fn csNewVmObject(vm: *cy.VM, typeId: cy.TypeId, argsPtr: [*]const Value, numArgs: usize) Value {
    const entry = &vm.types[typeId];
    std.debug.assert(entry.kind == .object);
    std.debug.assert(numArgs == entry.data.object.numFields);
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

export fn csSymbol(vm: *cy.VM, str: c.Str) Value {
    const id = vm.ensureSymbolExt(c.fromStr(str), true) catch fatal();
    return Value.initSymbol(@intCast(id));
}

export fn csNewPointer(vm: *cy.VM, ptr: ?*anyopaque) Value {
    return cy.heap.allocPointer(vm, ptr) catch fatal();
}

export fn csNewType(vm: *cy.VM, type_id: cy.TypeId) Value {
    return cy.heap.allocType(vm, type_id) catch fatal();
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
    try t.eq(c.toBool(c.float(0)), false);
    try t.eq(c.toBool(c.float(123.0)), true);
    try t.eq(c.toBool(c.integer(0)), false);
    try t.eq(c.toBool(c.integer(1)), true);
    try t.eq(c.toBool(c.csTrue()), true);
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

    var str = c.toStr("foo");
    var val = c.symbol(vm, str);
    try t.eq(c.asSymbolId(val), 0);

    str = c.toStr("bar");
    val = c.symbol(vm, str);
    try t.eq(c.asSymbolId(val), 1);

    str = c.toStr("foo");
    val = c.symbol(vm, str);
    try t.eq(c.asSymbolId(val), 0);
}

export fn csToTempString(vm: *cy.VM, val: Value) c.Str {
    const str = vm.getOrBufPrintValueStr(&cy.tempBuf, val) catch cy.fatal();
    return c.toStr(str);
}

export fn csToTempByteArray(vm: *cy.VM, val: Value) c.Str {
    const str = vm.getOrBufPrintValueRawStr(&cy.tempBuf, val) catch cy.fatal();
    return c.toStr(str);
}

test "Constants." {
    try t.eq(c.TypeVoid, bt.Void);
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
    try t.eq(c.TypeExternFunc, bt.ExternFunc);
    try t.eq(c.TypeType, bt.Type);
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

export fn csListGet(vm: *cy.VM, list: Value, idx: usize) Value {
    const res = list.asHeapObject().list.list.ptr[idx];
    vm.retain(res);
    return res;
}

export fn csListSet(vm: *cy.VM, list: Value, idx: usize, val: Value) void {
    vm.retain(val);
    vm.release(list.asHeapObject().list.list.ptr[idx]);
    list.asHeapObject().list.list.ptr[idx] = val;
}

export fn csListInsert(vm: *cy.VM, list: Value, idx: usize, val: Value) void {
    vm.retain(val);
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.asHeapObject().list.list);
    inner.growTotalCapacity(vm.alloc, inner.len + 1) catch cy.fatal();
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
    _ = c.eval(vm, c.toStr("[1, 2, 3]"), &list);
    defer c.release(vm, list);

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
    return c.toStr(build_options.full_version.ptr[0..build_options.full_version.len]);
}

test "csGetFullVersion()" {
    const str = c.getFullVersion();
    try t.eqStr(str.buf[0..str.len], build_options.full_version);
}

export fn csGetVersion() c.Str {
    return c.toStr(build_options.version.ptr[0..build_options.version.len]);
}

test "csGetVersion()" {
    const str = c.getVersion();
    try t.eqStr(str.buf[0..str.len], build_options.version);
}

export fn csGetBuild() c.Str {
    return c.toStr(build_options.build.ptr[0..build_options.build.len]);
}

test "csGetBuild()" {
    const str = c.getBuild();
    try t.eqStr(str.buf[0..str.len], build_options.build);
}

export fn csGetCommit() c.Str {
    return c.toStr(build_options.commit.ptr[0..build_options.commit.len]);
}

test "csGetCommit()" {
    const str = c.getCommit();
    try t.eqStr(str.buf[0..str.len], build_options.commit);
}

/// Used in C/C++ code to log synchronously.
pub export fn zig_log(buf: [*c]const u8) void {
    log.tracev("{s}", .{ buf });
}

pub export fn zig_log_u32(buf: [*c]const u8, val: u32) void {
    log.tracev("{s}: {}", .{ buf, val });
}

comptime {
    if (build_options.rt != .pm) {
        @export(cy.compiler.defaultModuleResolver, .{ .name = "csDefaultResolver", .linkage = .Strong });
        @export(cy.compiler.defaultModuleLoader, .{ .name = "csDefaultModuleLoader", .linkage = .Strong });
    }
}