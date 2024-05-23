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
const vmc = cy.vmc;

const cli = @import("cli.zig");

comptime {
    if (build_options.cli or builtin.os.tag == .wasi) {
        std.testing.refAllDecls(cli);
    }
}

export var clVerbose = false;
export var clSilent = false;

export fn clCreate() *cy.VM {
    const alloc = cy.heap.getAllocator();
    const vm = alloc.create(cy.VM) catch fatal();
    vm.init(alloc) catch fatal();
    return @ptrCast(vm);
}

export fn clDeinit(vm: *cy.VM) void {
    vm.deinitRtObjects();
    vm.compiler.deinitModRetained();
}

export fn clDestroy(vm: *cy.VM) void {
    vm.deinit(false);
    const alloc = cy.heap.getAllocator();
    alloc.destroy(vm);
}

export fn clGetGlobalRC(vm: *cy.VM) usize {
    return cy.arc.getGlobalRC(vm);
}

export fn clCountObjects(vm: *cy.VM) usize {
    return cy.arc.countObjects(vm);
}

export fn clTraceDumpLiveObjects(vm: *cy.VM) void {
    if (cy.Trace) {
        var iter = vm.objectTraceMap.iterator();
        while (iter.next()) |it| {
            const trace = it.value_ptr.*;
            if (trace.free_pc == null) {
                cy.arc.dumpObjectAllocTrace(vm, it.key_ptr.*, trace.alloc_pc) catch fatal();
            }
        }
    }
}

/// This is useful when calling into wasm to allocate some memory.
export fn clAlloc(vm: *cy.VM, size: usize) [*]const u8 {
    const slice = vm.alloc.alignedAlloc(u8, 8, size) catch fatal();
    return slice.ptr;
}

export fn clFree(vm: *cy.VM, bytes: c.Str) void {
    vm.alloc.free(bytes.ptr[0..bytes.len]);
}

export fn clFreeZ(vm: *cy.VM, str: [*:0]const u8) void {
    vm.alloc.free(std.mem.sliceTo(str, 0));
}

export fn clGetAllocator(vm: *cy.VM) c.Allocator {
    return c.toAllocator(vm.alloc);
}

export fn clDefaultEvalConfig() c.EvalConfig {
    return .{
        .single_run = false,
        .file_modules = false,
        .reload = false,
        .backend = c.BackendVM,
        .gen_all_debug_syms = false,
        .spawn_exe = false, 
    };
}

export fn clSetNewMockHttp(vm: *cy.VM) *anyopaque {
    const client = vm.alloc.create(cy.http.MockHttpClient) catch fatal();
    client.* = cy.http.MockHttpClient.init(vm.alloc);
    vm.httpClient = client.iface();
    return client;
}

export fn clReset(vm: *cy.VM) void {
    vm.resetVM() catch cy.fatal();
}

export fn clEval(vm: *cy.VM, src: c.Str, outVal: *cy.Value) c.ResultCode {
    const uri: []const u8 = "main";
    return clEvalExt(vm, c.toStr(uri), src, c.defaultEvalConfig(), outVal);
}

export fn clEvalExt(vm: *cy.VM, uri: c.Str, src: c.Str, config: c.EvalConfig, outVal: *cy.Value) c.ResultCode {
    var res = c.Success;
    outVal.* = vm.eval(c.fromStr(uri), c.fromStr(src), config) catch |err| b: {
        switch (err) {
            error.CompileError => {
                res = c.ErrorCompile;
            },
            error.Panic => {
                res = c.ErrorPanic;
            },
            error.Await => {
                res = c.Await;
            },
            else => {
                log.tracev("{}", .{err});
                res = c.ErrorUnknown;
            },
        }
        break :b undefined;
    };
    vm.last_res = res;
    return res;
}

export fn clEvalPath(vm: *cy.VM, uri: c.Str, config: c.EvalConfig, outVal: *cy.Value) c.ResultCode {
    var res = c.Success;
    outVal.* = vm.eval(c.fromStr(uri), null, config) catch |err| b: {
        switch (err) {
            error.CompileError => {
                res = c.ErrorCompile;
            },
            error.Panic => {
                res = c.ErrorPanic;
            },
            error.Await => {
                res = c.Await;
            },
            else => {
                log.tracev("{}", .{err});
                res = c.ErrorUnknown;
            },
        }
        break :b undefined;
    };
    vm.last_res = res;
    return res;
}

export fn clRunReadyTasks(vm: *cy.VM) c.ResultCode {
    var res = c.Success;
    runReadyTasks(vm) catch |err| {
        switch (err) {
            error.CompileError => {
                res = c.ErrorCompile;
            },
            error.Panic => {
                res = c.ErrorPanic;
            },
            error.Await => {
                res = c.Await;
            },
            else => {
                log.tracev("{}", .{err});
                res = c.ErrorUnknown;
            },
        }
    };
    vm.last_res = res;
    return res;
}

fn runReadyTasks(vm: *cy.VM) anyerror!void {
    log.tracev("run ready tasks", .{});

    while (vm.ready_tasks.readItem()) |task| {
        switch (task.type) {
            .cont => {
                const fiber = task.data.cont;
                vm.c.curFiber = fiber;
                vm.c.stack = @ptrCast(fiber.stackPtr);
                vm.c.stack_len = fiber.stackLen;
                vm.c.stackEndPtr = vm.c.stack + fiber.stackLen;
                vm.c.framePtr = vm.c.stack + fiber.stackOffset;
                vm.c.pc = vm.c.ops + fiber.pcOffset;
                @call(.never_inline, cy.vm.evalLoopGrowStack, .{vm, true}) catch |err| {
                    if (err == error.Await) {
                        // Nop.
                    } else {
                        return err;
                    }
                };

                // Release fiber.
                vm.releaseObject(@ptrCast(fiber));
            },
            .callback => {
                // Create fiber.
                const arg_dst = 1;
                const initial_stack_size: u32 = 16;
                const fiberv = try cy.fiber.allocFiber(vm, cy.NullId, &.{}, arg_dst, initial_stack_size);
                const fiber: *vmc.Fiber = @ptrCast(fiberv.asHeapObject());
                vm.c.curFiber = fiber;
                vm.c.pc = vm.c.ops;
                vm.c.framePtr = @ptrCast(fiber.stackPtr);
                vm.c.stack = @ptrCast(fiber.stackPtr);
                vm.c.stack_len = fiber.stackLen;
                vm.c.stackEndPtr = @ptrCast(fiber.stackPtr + fiber.stackLen);
                _ = vm.callFunc(task.data.callback, &.{}, .{ .from_external = false }) catch |err| {
                    if (err == error.Await) {
                        return error.TODO;
                    } else {
                        return err;
                    }
                };

                cy.fiber.saveCurFiber(vm);
                fiber.pcOffset = cy.NullId;

                // Release callback.
                vm.release(task.data.callback);

                // Release fiber.
                vm.release(fiberv);
            },
        }
    }
}

export fn clDefaultCompileConfig() c.CompileConfig {
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

export fn clCompile(vm: *cy.VM, uri: c.Str, src: c.Str, config: c.CompileConfig) c.ResultCode {
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

export fn clValidate(vm: *cy.VM, src: c.Str) c.ResultCode {
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

test "clValidate()" {
    const vm = c.create();
    defer c.destroy(vm);

    c.setSilent(true);
    defer c.setSilent(false);

    var res = c.validate(vm, c.toStr("1 + 2"));
    try t.eq(res, c.Success);

    res = c.validate(vm, c.toStr("1 +"));
    try t.eq(res, c.ErrorCompile);
}

export fn clResultName(code: c.ResultCode) c.Str {
    switch (code) {
        c.Success => return c.toStr("Success"),
        c.ErrorCompile => return c.toStr("CompileError"),
        c.ErrorPanic => return c.toStr("PanicError"),
        c.ErrorUnknown => return c.toStr("UnknownError"),
        else => fatal(),
    }
}

var tempBuf: [1024]u8 align(8) = undefined;

export fn clNewLastErrorSummary(vm: *cy.VM) c.Str {
    if (vm.last_res == c.ErrorCompile) {
        return clNewErrorReportSummary(vm);
    } else if (vm.last_res == c.ErrorPanic) {
        return clNewPanicSummary(vm);
    } else {
        return c.NullStr;
    }
}

export fn clNewErrorReportSummary(vm: *cy.VM) c.Str {
    if (vm.compiler.reports.items.len == 0) {
        cy.panic("No report.");
    }
    const str = cy.debug.allocReportSummary(vm.compiler, vm.compiler.reports.items[0]) catch fatal();
    return c.toStr(str);
}

export fn clNewPanicSummary(vm: *const cy.VM) c.Str {
    if (vm.config.backend == c.BackendVM) {
        const summary = cy.debug.allocLastUserPanicError(vm) catch fatal();
        return c.toStr(summary);
    } else {
        const dupe = vm.alloc.dupe(u8, vm.lastExeError) catch fatal();
        return c.toStr(dupe);
    }
}

export fn clReportApiError(vm: *const cy.VM, msg: c.Str) void {
    vm.setApiError(c.fromStr(msg)) catch fatal();
}

export fn clGetResolver(vm: *cy.VM) c.ResolverFn {
    return vm.compiler.moduleResolver;
}

export fn clSetResolver(vm: *cy.VM, resolver: c.ResolverFn) void {
    vm.compiler.moduleResolver = resolver;
}

export fn clResolve(vm: *cy.VM, uri: c.Str) c.Str {
    var buf: [4096]u8 = undefined;
    const r_uri_temp = cy.compiler.resolveModuleUri(vm.compiler, &buf, c.fromStr(uri)) catch fatal();
    const r_uri = vm.alloc.dupe(u8, r_uri_temp) catch fatal();
    return c.toStr(r_uri);
}

export fn clGetModuleLoader(vm: *cy.VM) c.ModuleLoaderFn {
    return vm.compiler.moduleLoader;
}

export fn clSetModuleLoader(vm: *cy.VM, loader: c.ModuleLoaderFn) void {
    vm.compiler.moduleLoader = loader;
}

export fn clGetPrinter(vm: *cy.VM) c.PrintFn {
    return vm.print;
}

export fn clSetPrinter(vm: *cy.VM, print: c.PrintFn) void {
    vm.print = print;
}

export fn clGetErrorPrinter(vm: *cy.VM) c.PrintErrorFn {
    return vm.print_err;
}

export fn clSetErrorPrinter(vm: *cy.VM, print: c.PrintErrorFn) void {
    vm.print_err = print;
}

pub export var clLog: c.LogFn = cy.log.defaultLog;

export fn clPerformGC(vm: *cy.VM) c.GCResult {
    const res = cy.arc.performGC(vm) catch cy.fatal();
    return .{
        .numCycFreed = res.numCycFreed,
        .numObjFreed = res.numObjFreed,
    };
}

export fn clDeclareDynFunc(mod: c.Sym, name: [*:0]const u8, numParams: u32, funcPtr: c.FuncFn) void {
    const modSym = cy.Sym.fromC(mod);
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = modSym.cast(.chunk).getMod().chunk;
    const funcSigId = chunk.sema.ensureUntypedFuncSig(numParams) catch fatal();
    if (modSym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const func = chunk.reserveHostFunc(modSym, symName, null, false, false) catch cy.fatal();
    chunk.resolveHostFunc(func, funcSigId, @ptrCast(funcPtr)) catch cy.fatal();
    if (nameOwned) {
        const sym = func.sym.?;
        sym.head.setNameOwned(true);
    }
}

export fn clDeclareFunc(mod: c.Sym, name: [*:0]const u8, params: [*]const cy.TypeId, numParams: usize, retType: cy.TypeId, funcPtr: c.FuncFn) void {
    const modSym = cy.Sym.fromC(mod);
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = modSym.cast(.chunk).getMod().chunk;
    const funcSigId = chunk.sema.ensureFuncSig(params[0..numParams], retType) catch cy.fatal();
    if (modSym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const func = chunk.reserveHostFunc(modSym, symName, null, false, false) catch cy.fatal();
    chunk.resolveHostFunc(func, funcSigId, @ptrCast(funcPtr)) catch cy.fatal();
    if (nameOwned) {
        const sym = func.sym.?;
        sym.head.setNameOwned(true);
    }
}

export fn clDeclareDynVar(mod: c.Sym, name: [*:0]const u8, val: c.Value) void {
    clDeclareVar(mod, name, bt.Dyn, val);
}

export fn clDeclareVar(mod: c.Sym, name: [*:0]const u8, typeId: cy.TypeId, val: c.Value) void {
    const modSym = cy.Sym.fromC(mod);
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = modSym.cast(.chunk).getMod().chunk;
    if (modSym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const sym = chunk.reserveHostVar(modSym, symName, null) catch cy.fatal();
    chunk.resolveHostVar(sym, typeId, @bitCast(val)) catch cy.fatal();
    if (nameOwned) {
        sym.head.setNameOwned(true);
    }
}

export fn clExpandTemplateType(ctemplate: c.Sym, args_ptr: [*]const cy.Value, nargs: u32) c.TypeId {
    const template = cy.Sym.fromC(ctemplate).cast(.template);
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

export fn clRelease(vm: *cy.VM, val: Value) void {
    vm.release(val);
}

export fn clRetain(vm: *cy.VM, val: Value) void {
    vm.retain(val);
}

export fn clGetUserData(vm: *cy.VM) ?*anyopaque {
    return vm.userData;
}

export fn clSetUserData(vm: *cy.VM, userData: ?*anyopaque) void {
    vm.userData = userData;
}

export fn clTrue() Value {
    return Value.True;
}

export fn clFalse() Value {
    return Value.False;
}

export fn clBool(b: bool) Value {
    return Value.initBool(b);
}

export fn clFloat(n: f64) Value {
    return Value.initF64(n);
}

export fn clInteger(n: i64) Value {
    return Value.initInt(@intCast(n));
}

export fn clInteger32(n: i32) Value {
    return Value.initInt(n);
}

export fn clHostObject(ptr: *anyopaque) Value {
    return Value.initHostPtr(ptr);
}

export fn clVmObject(ptr: *anyopaque) Value {
    return Value.initPtr(ptr);
}

export fn clNewString(vm: *cy.VM, cstr: c.Str) Value {
    return vm.allocString(c.fromStr(cstr)) catch fatal();
}

export fn clNewAstring(vm: *cy.VM, cstr: c.Str) Value {
    return vm.retainOrAllocAstring(c.fromStr(cstr)) catch fatal();
}

export fn clNewUstring(vm: *cy.VM, cstr: c.Str) Value {
    return vm.retainOrAllocUstring(c.fromStr(cstr)) catch fatal();
}

export fn clNewTuple(vm: *cy.VM, ptr: [*]const Value, len: usize) Value {
    const elems = ptr[0..len];
    for (elems) |elem| {
        cy.arc.retain(vm, elem);
    }
    return cy.heap.allocTuple(vm, elems) catch fatal();
}

export fn clNewEmptyListDyn(vm: *cy.VM) Value {
    return vm.allocEmptyListDyn() catch fatal();
}

export fn clNewListDyn(vm: *cy.VM, ptr: [*]const Value, len: usize) Value {
    const elems = ptr[0..len];
    for (elems) |elem| {
        cy.arc.retain(vm, elem);
    }
    return cy.heap.allocListDyn(vm, elems) catch fatal();
}

export fn clNewEmptyMap(vm: *cy.VM) Value {
    return cy.heap.allocEmptyMap(vm) catch fatal();
}

export fn clNewFuncDyn(vm: *cy.VM, numParams: u32, func: c.FuncFn) Value {
    const funcSigId = vm.sema.ensureUntypedFuncSig(numParams) catch fatal();
    return vm.allocHostFunc(@ptrCast(func), numParams, funcSigId, null, false) catch fatal();
}

export fn clNewFunc(vm: *cy.VM, params: [*]const cy.TypeId, numParams: u32, retType: cy.TypeId, func: c.FuncFn) Value {
    const funcSigId = vm.sema.ensureFuncSig(params[0..numParams], retType) catch fatal();
    const funcSig = vm.sema.funcSigs.items[funcSigId];
    return vm.allocHostFunc(@ptrCast(func), numParams, funcSigId, null, funcSig.reqCallTypeCheck) catch fatal();
}

test "clNewFunc()" {
    const vm = c.create();
    defer c.destroy(vm);

    const val = c.newFunc(vm, &[_]cy.TypeId{}, 0, bt.Dyn, @ptrFromInt(8));
    try t.eq(c.getTypeId(val), bt.HostFunc);
}

export fn clCreateModule(vm: *cy.VM, r_uri: c.Str, src: c.Str) c.Module {
    const chunk = cy.compiler.createModule(vm.compiler, c.fromStr(r_uri), c.fromStr(src)) catch @panic("error");
    return .{ .ptr = @ptrCast(chunk) };
}

export fn clSetModuleConfig(vm: *cy.VM, mod: c.Module, config: *c.ModuleConfig) void {
    const chunk = cy.Chunk.fromC(mod);

    chunk.varLoader = config.varLoader;
    chunk.onTypeLoad = config.onTypeLoad;
    chunk.onLoad = config.onLoad;
    chunk.onDestroy = config.onDestroy;

    // Allocate func mapping.
    var funcs: std.StringHashMapUnmanaged(c.FuncFn) = .{};
    for (c.fromSlice(c.HostFuncEntry, config.funcs)) |entry| {
        funcs.put(vm.alloc, c.fromStr(entry.name), entry.func) catch @panic("error"); 
    }
    chunk.host_funcs.deinit(vm.alloc);
    chunk.host_funcs = funcs;
    chunk.func_loader = config.func_loader;

    // Allocate host type mapping.
    var types: std.StringHashMapUnmanaged(c.HostType) = .{};
    for (c.fromSlice(c.HostTypeEntry, config.types)) |entry| {
        types.put(vm.alloc, c.fromStr(entry.name), entry.host_t) catch @panic("error"); 
    }
    chunk.host_types.deinit(vm.alloc);
    chunk.host_types = types;
    chunk.type_loader = config.type_loader;
}

export fn clNewHostObject(vm: *cy.VM, typeId: cy.TypeId, size: usize) Value {
    const ptr = cy.heap.allocHostCycObject(vm, typeId, size) catch cy.fatal();
    return Value.initHostCycPtr(ptr);
}

export fn clNewHostObjectPtr(vm: *cy.VM, typeId: cy.TypeId, size: usize) *anyopaque {
    return cy.heap.allocHostCycObject(vm, typeId, size) catch cy.fatal();
}

export fn clNewVmObject(vm: *cy.VM, typeId: cy.TypeId, argsPtr: [*]const Value, numArgs: usize) Value {
    const entry = &vm.c.types[typeId];
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

export fn clSymbol(vm: *cy.VM, str: c.Str) Value {
    const id = vm.ensureSymbolExt(c.fromStr(str), true) catch fatal();
    return Value.initSymbol(@intCast(id));
}

export fn clNewPointer(vm: *cy.VM, ptr: ?*anyopaque) Value {
    return cy.heap.allocPointer(vm, ptr) catch fatal();
}

export fn clNewType(vm: *cy.VM, type_id: cy.TypeId) Value {
    return cy.heap.allocType(vm, type_id) catch fatal();
}

test "clNewPointer()" {
    const vm = c.create();
    defer c.destroy(vm);

    const val = c.newPointer(vm, @ptrFromInt(123));
    try t.eq(c.getTypeId(val), bt.Pointer);

    const obj = (Value{.val = val}).asHeapObject();
    try t.eq(@intFromPtr(obj.pointer.ptr), 123);
}

export fn clAsFloat(val: Value) f64 {
    return val.asF64();
}

test "clAsFloat()" {
    try t.eq(c.asFloat(c.float(123.0)), 123);
}

export fn clToBool(val: Value) bool {
    return val.toBool();
}

test "clToBool()" {
    try t.eq(c.toBool(c.float(0)), false);
    try t.eq(c.toBool(c.float(123.0)), true);
    try t.eq(c.toBool(c.integer(0)), false);
    try t.eq(c.toBool(c.integer(1)), true);
    try t.eq(c.toBool(c.clTrue()), true);
    try t.eq(c.toBool(c.clFalse()), false);
}

export fn clAsBool(val: Value) bool {
    return val.asBool();
}

test "clAsBool()" {
    try t.eq(c.asBool(c.clTrue()), true);
    try t.eq(c.asBool(c.clFalse()), false);
}

export fn clAsInteger(val: Value) i64 {
    return val.asInteger();
}

test "clAsInteger()" {
    try t.eq(c.asInteger(c.integer(123)), 123);
}

export fn clAsSymbolId(val: Value) u32 {
    return val.asSymbolId();
}

test "clAsSymbolId()" {
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

export fn clToTempString(vm: *cy.VM, val: Value) c.Str {
    const str = vm.getOrBufPrintValueStr(&cy.tempBuf, val) catch cy.fatal();
    return c.toStr(str);
}

export fn clToTempByteArray(vm: *cy.VM, val: Value) c.Str {
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
    try t.eq(c.TypeListDyn, bt.ListDyn);
    try t.eq(c.TypeListIterDyn, bt.ListIterDyn);
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

export fn clAsHostObject(val: Value) *anyopaque {
    return val.castHostObject(*anyopaque);
}

export fn clGetTypeId(val: Value) c.TypeId {
    return val.getTypeId();
}

test "clGetTypeId()" {
    try t.eq(c.getTypeId(c.float(123)), bt.Float);
}

export fn clNewValueDump(vm: *cy.VM, val: Value) c.Str {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    const w = buf.writer(vm.alloc);
    cy.debug.dumpValue(vm, w, val, .{}) catch cy.fatal();
    return c.toStr(buf.toOwnedSlice(vm.alloc) catch cy.fatal());
}

export fn clListLen(list: Value) usize {
    return list.asHeapObject().list.list.len;
}

export fn clListCap(list: Value) usize {
    return list.asHeapObject().list.list.cap;
}

export fn clListGet(vm: *cy.VM, list: Value, idx: usize) Value {
    const res = list.asHeapObject().list.list.ptr[idx];
    vm.retain(res);
    return res;
}

export fn clListSet(vm: *cy.VM, list: Value, idx: usize, val: Value) void {
    vm.retain(val);
    vm.release(list.asHeapObject().list.list.ptr[idx]);
    list.asHeapObject().list.list.ptr[idx] = val;
}

export fn clListInsert(vm: *cy.VM, list: Value, idx: usize, val: Value) void {
    vm.retain(val);
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.asHeapObject().list.list);
    inner.growTotalCapacity(vm.alloc, inner.len + 1) catch cy.fatal();
    inner.insertAssumeCapacity(idx, val);
}

export fn clListAppend(vm: *cy.VM, list: Value, val: Value) void {
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

export fn clGetFullVersion() c.Str {
    return c.toStr(build_options.full_version);
}

test "clGetFullVersion()" {
    const str = c.getFullVersion();
    try t.eqStr(c.fromStr(str), build_options.full_version);
}

export fn clGetVersion() c.Str {
    return c.toStr(build_options.version);
}

test "clGetVersion()" {
    const str = c.getVersion();
    try t.eqStr(c.fromStr(str), build_options.version);
}

export fn clGetBuild() c.Str {
    return c.toStr(build_options.build);
}

test "clGetBuild()" {
    const str = c.getBuild();
    try t.eqStr(c.fromStr(str), build_options.build);
}

export fn clGetCommit() c.Str {
    return c.toStr(build_options.commit);
}

test "clGetCommit()" {
    const str = c.getCommit();
    try t.eqStr(c.fromStr(str), build_options.commit);
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
        @export(cy.compiler.defaultModuleResolver, .{ .name = "clDefaultResolver", .linkage = .strong });
        @export(cy.compiler.defaultModuleLoader, .{ .name = "clDefaultModuleLoader", .linkage = .strong });
    }
}