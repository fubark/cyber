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
    vm.compiler.deinitValues();
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
    const uri: []const u8 = "eval";
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
                while (true) {
                    _ = vm.callFunc(task.data.callback, &.{}, .{ .from_external = false }) catch |err| {
                        if (err == error.Await) {
                            return error.TODO;
                        } else if (err == error.StackOverflow) {
                            try @call(.never_inline, cy.fiber.growStackAuto, .{vm});
                            continue;
                        } else {
                            return err;
                        }
                    };
                    break;
                }

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
    defer vm.destroy();

    c.setSilent(true);
    defer c.setSilent(false);

    var res = vm.validate("1 + 2");
    try t.eq(res, c.Success);

    res = vm.validate("1 +");
    try t.eq(res, c.ErrorCompile);
}

export fn clGetInt(vm: *cy.VM, idx: u32) i64 {
    return vm.getInt(idx);
}

export fn clGetFloat(vm: *cy.VM, idx: u32) f64 {
    return vm.getFloat(idx);
}

export fn clGetValue(vm: *cy.VM, idx: u32) Value {
    return vm.getValue(idx);
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

export fn clDeclareFuncDyn(mod: c.Sym, name: [*:0]const u8, numParams: u32, funcPtr: c.FuncFn) void {
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
        const sym = func.parent.cast(.func);
        sym.head.setNameOwned(true);
    }
}

export fn clDeclareFunc(mod: c.Sym, name: [*:0]const u8, params: [*]const cy.TypeId, numParams: usize, retType: cy.TypeId, funcPtr: c.FuncFn) void {
    const modSym = cy.Sym.fromC(mod);
    var symName: []const u8 = std.mem.sliceTo(name, 0);
    var nameOwned = false;
    const chunk = modSym.cast(.chunk).getMod().chunk;
    const funcSigId = chunk.sema.ensureFuncSig(@ptrCast(params[0..numParams]), retType) catch cy.fatal();
    if (modSym.getMod().?.getSym(symName) == null) {
        symName = chunk.alloc.dupe(u8, symName) catch fatal();
        nameOwned = true;
    }
    const func = chunk.reserveHostFunc(modSym, symName, null, false, false) catch cy.fatal();
    chunk.resolveHostFunc(func, funcSigId, @ptrCast(funcPtr)) catch cy.fatal();
    if (nameOwned) {
        const sym = func.parent.cast(.func);
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

export fn clFindType(vm: *cy.VM, c_path: c.Str) cy.TypeId {
    const path = c.fromStr(c_path);
    if (vm.compiler.chunks.items.len == 0) {
        return bt.Null;
    }

    // Look in main first.
    const main_mod = vm.compiler.main_chunk.sym.getMod();
    if (main_mod.getSym(path)) |sym| {
        if (sym.getStaticType()) |type_id| {
            return type_id;
        }
    }

    // Look in builtins.
    const b_mod = vm.compiler.chunks.items[0].sym.getMod();
    const sym = b_mod.getSym(path) orelse {
        return bt.Null;
    };
    return sym.getStaticType() orelse {
        return bt.Null;
    };
}

test "clFindType()" {
    const vm = c.create();
    defer vm.destroy();

    // Before types are loaded.
    try t.eq(vm.findType("int"), c.TypeNull);

    // Load builtin type.
    _ = vm.compile("main", "", c.defaultCompileConfig());
    try t.eq(vm.findType("int"), c.TypeInteger);

    // Load type declared from main.
    _ = vm.compile("main",
        \\type Foo:
        \\    a int
    , c.defaultCompileConfig());
    try t.expect(vm.findType("Foo") != c.TypeNull);
}

export fn clExpandTemplateType(vm: *cy.VM, ctemplate: c.Sym, args_ptr: [*]const cy.Value, nargs: usize, res: *c.Type) bool {
    _ = vm;
    const template = cy.Sym.fromC(ctemplate).cast(.template);
    const args = args_ptr[0..nargs];
    const type_id = clExpandTemplateType2(template, args) catch |err| {
        log.tracev("{}", .{err});
        return false;
    };
    res.* = type_id;
    return true;
}

fn clExpandTemplateType2(template: *cy.sym.Template, args: []const cy.Value) !cy.TypeId {
    const chunk = template.chunk();

    // Build args types.
    const typeStart = chunk.typeStack.items.len;
    defer chunk.typeStack.items.len = typeStart;
    for (args) |arg| {
        try chunk.typeStack.append(chunk.alloc, arg.getTypeId());
    }
    const arg_types = chunk.typeStack.items[typeStart..];

    // Check against template signature.
    if (!cy.types.isTypeFuncSigCompat(chunk.compiler, @ptrCast(arg_types), .not_void, template.sigId)) {
        return error.SigMismatch;
    }

    const sym = try cy.cte.expandTemplate(chunk, template, args);
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

export fn clInt(n: i64) Value {
    return Value.initInt(@intCast(n));
}

export fn clInt32(n: i32) Value {
    return Value.initInt(n);
}

export fn clHostObject(ptr: *anyopaque) Value {
    return Value.initHostPtr(ptr);
}

export fn clVmObject(ptr: *anyopaque) Value {
    return Value.initPtr(ptr);
}

export fn clNewInt(vm: *cy.VM, n: i64) Value {
    return vm.allocInt(n) catch fatal();
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

export fn clNewList(vm: *cy.VM, list_t: *cy.Type, ptr: [*]const Value, len: usize) Value {
    const elems = ptr[0..len];
    for (elems) |elem| {
        cy.arc.retain(vm, elem);
    }
    return vm.newList(list_t, elems) catch fatal();
}

export fn clNewEmptyMap(vm: *cy.VM) Value {
    return cy.heap.allocEmptyMap(vm) catch fatal();
}

export fn clNewFuncDyn(vm: *cy.VM, numParams: u32, func: c.FuncFn) Value {
    const funcSigId = vm.sema.ensureUntypedFuncSig(numParams) catch fatal();
    return vm.allocHostFuncUnion(bt.Func, @ptrCast(func), numParams, funcSigId, null, false) catch fatal();
}

export fn clNewFunc(vm: *cy.VM, params: [*]const cy.TypeId, numParams: usize, retType: cy.TypeId, func: c.FuncFn) Value {
    const funcSigId = vm.sema.ensureFuncSig(@ptrCast(params[0..numParams]), retType) catch fatal();
    const funcSig = vm.sema.funcSigs.items[funcSigId];
    return vm.allocHostFuncUnion(bt.Func, @ptrCast(func), numParams, funcSigId, null, funcSig.info.reqCallTypeCheck) catch fatal();
}

test "clNewFunc()" {
    const vm = c.create();
    defer vm.destroy();

    const val = vm.newFunc(&.{}, bt.Dyn, @ptrFromInt(8));
    try t.eq(c.getType(val), bt.Func);
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

export fn clNewInstance(vm: *cy.VM, type_id: cy.TypeId, fields: [*]const c.FieldInit, nfields: usize) cy.Value {
    const type_e = vm.c.types[type_id];
    if (type_e.kind != .object and type_e.kind != .struct_t) {
        cy.panicFmt("Expected object or struct type. Found `{}`.", .{type_e.kind});
    }

    const start = vm.compiler.main_chunk.valueStack.items.len;
    defer vm.compiler.main_chunk.valueStack.items.len = start;
    const total_fields = if (type_e.kind == .object) type_e.data.object.numFields else type_e.data.struct_t.nfields;
    vm.compiler.main_chunk.valueStack.resize(vm.alloc, start + total_fields) catch cy.fatal();
    const args = vm.compiler.main_chunk.valueStack.items[start..];
    @memset(args, Value.Void);

    const mod = type_e.sym.getMod().?;
    for (fields[0..nfields]) |field| {
        const name = c.fromStr(field.name);
        const sym = mod.getSym(name) orelse {
            cy.panicFmt("No such field `{s}`.", .{name});
        };
        if (sym.type != .field) {
            cy.panicFmt("`{s}` is not a field.", .{name});
            return vm.prepPanic("Not a field.");
        }
        args[sym.cast(.field).idx] = @bitCast(field.value);
    }

    if (args.len <= 4) {
        return cy.heap.allocObjectSmall(vm, type_id, args) catch cy.fatal();
    } else {
        return cy.heap.allocObject(vm, type_id, args) catch cy.fatal();
    }
}

test "clNewInstance()" {
    const vm = c.create();
    defer vm.destroy();

    var res: c.Value = undefined;
    vm.evalMust( 
        \\type Foo:
        \\    a int
        \\    b String
    , &res);
    const foo_t = vm.findType("Foo");
    const val = vm.newInstance(foo_t, &.{
        c.toFieldInit("a", c.int(123)),
        c.toFieldInit("b", vm.newString("abc")),
    });
    try t.eq(c.getType(val), foo_t);

    try t.eq(vm.getField(val, "a"), 123);
    try t.eqStr(c.asString(vm.getField(val, "b")), "abc");
}

export fn clNewChoice(vm: *cy.VM, choice_t: cy.TypeId, name: c.Str, val: cy.Value) cy.Value {
    const type_e = vm.c.types[choice_t];
    if (type_e.kind != .choice) {
        return cy.panicFmt("Expected choice type. Found `{}`.", .{type_e.kind});
    }

    const zname = c.fromStr(name);
    const sym = type_e.sym.getMod().?.getSym(zname) orelse {
        return cy.panicFmt("Can not find case `{s}`.", .{zname});
    };
    if (sym.type != .enumMember) {
        return cy.panicFmt("`{s}` is not choice case.", .{zname});
    }
    const case = sym.cast(.enumMember);
    return cy.heap.allocObjectSmall(vm, choice_t, &.{
        @bitCast(c.int(@intCast(case.val))),
        val,
    }) catch cy.fatal();
}

test "clNewChoice()" {
    const vm = c.create();
    defer vm.destroy();

    var res: c.Value = undefined;
    vm.evalMust( 
        \\type Foo enum:
        \\    case a int
        \\    case b String
    , &res);
    const foo_t = vm.findType("Foo");
    const val = vm.newChoice(foo_t, "a", c.int(123));
    try t.eq(c.getType(val), foo_t);
    try t.eq(vm.unwrapChoice(val, "a"), 123);
}

export fn clSymbol(vm: *cy.VM, str: c.Str) Value {
    const id = vm.ensureSymbolExt(c.fromStr(str), true) catch fatal();
    return Value.initSymbol(@intCast(id));
}

export fn clGetField(vm: *cy.VM, val: cy.Value, name: c.Str) cy.Value {
    return vm.getFieldName(val, c.fromStr(name)) catch {
        return cy.panicFmt("Can not access field: `{s}`", .{c.fromStr(name)});
    };
}

test "clGetField()" {
    const vm = c.create();
    defer vm.destroy();

    var res: c.Value = undefined;
    vm.evalMust( 
        \\type Foo:
        \\    a int
        \\    b String
        \\Foo{a=123, b='abc'}
    , &res);
    defer vm.release(res);
    try t.eq(vm.getField(res, "a"), 123);
    try t.eqStr(c.asString(vm.getField(res, "b")), "abc");
}

export fn clUnwrapChoice(vm: *cy.VM, choice: cy.Value, name: c.Str) cy.Value {
    const type_e = vm.sema.getType(choice.getTypeId());
    if (type_e.kind != .choice) {
        return cy.panicFmt("Expected a choice type. Found `{}`", .{type_e.kind});
    }

    const zname = c.fromStr(name);
    const sym = type_e.sym.getMod().?.getSym(zname) orelse {
        return cy.panicFmt("Can not find case `{s}`.", .{zname});
    };
    if (sym.type != .enumMember) {
        return cy.panicFmt("`{s}` is not choice case.", .{zname});
    }
    const case = sym.cast(.enumMember);

    const active_tag = choice.asHeapObject().object.getValue(0).asInt();
    if (active_tag != case.val) {
        return cy.panicFmt("Expected active tag `{}` for `{s}`. Found `{}`.", .{case.val, zname, active_tag});
    }

    const payload = choice.asHeapObject().object.getValue(1);
    if (!vm.sema.isUnboxedType(case.payloadType)) {
        vm.retain(payload);
    }
    return payload;
}

// To enable logging for tests:
// c.setVerbose(true);
// c.setLog(printLogger);
fn printLogger(str: c.Str) callconv(.C) void {
    std.debug.print("{s}\n", .{ c.fromStr(str) });
}

test "clUnwrapChoice()" {
    const vm = c.create();
    defer vm.destroy();

    var res: c.Value = undefined;
    vm.evalMust( 
        \\type Foo enum:
        \\    case a int
        \\    case b String
        \\Foo.a(123)
    , &res);
    defer vm.release(res);
    try t.eq(vm.unwrapChoice(res, "a"), 123);
}

export fn clNewPointerVoid(vm: *cy.VM, ptr: ?*anyopaque) Value {
    const bt_data = vm.getData(*cy.builtins.BuiltinsData, "builtins");
    return cy.heap.allocPointer(vm, bt_data.PtrVoid, ptr) catch fatal();
}

export fn clNewType(vm: *cy.VM, type_id: cy.TypeId) Value {
    return cy.heap.allocType(vm, type_id) catch fatal();
}

test "clNewPointer()" {
    const vm = c.create();
    defer vm.destroy();

    const val = vm.newPointerVoid(@ptrFromInt(123));
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
    const vm = c.create();
    defer vm.destroy();

    try t.eq(c.toBool(c.float(0)), false);
    try t.eq(c.toBool(c.float(123.0)), true);

    var i = vm.newInt(0);
    try t.eq(c.toBool(i), false);
    i = vm.newInt(1);
    try t.eq(c.toBool(i), true);

    try t.eq(c.toBool(c.True), true);
    try t.eq(c.toBool(c.False), false);
}

export fn clAsBool(val: Value) bool {
    return val.asBool();
}

test "clAsBool()" {
    try t.eq(c.asBool(c.True), true);
    try t.eq(c.asBool(c.False), false);
}

export fn clAsBoxInt(val: Value) i64 {
    return val.asBoxInt();
}

test "clAsBoxInt()" {
    const vm = c.create();
    defer vm.destroy();

    const val = vm.newInt(123);
    try t.eq(c.asBoxInt(val), 123);
}

export fn clAsString(val: cy.Value) c.Str {
    return c.toStr(val.asString());
}

export fn clAsSymbolId(val: Value) u32 {
    return val.asSymbolId();
}

test "clAsSymbolId()" {
    const vm = c.create();
    defer vm.destroy();

    var val = c.symbol(vm, "foo");
    try t.eq(c.asSymbolId(val), 49);

    val = c.symbol(vm, "bar");
    try t.eq(c.asSymbolId(val), 50);

    val = c.symbol(vm, "foo");
    try t.eq(c.asSymbolId(val), 49);
}

export fn clToTempString(vm: *cy.VM, val: Value) c.Str {
    const str = vm.getOrBufPrintValueStr(&cy.tempBuf, val) catch cy.fatal();
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
    try t.eq(c.TypeFunc, bt.Func);
    try t.eq(c.TypeString, bt.String);
    try t.eq(c.TypeFiber, bt.Fiber);
    try t.eq(c.TypeExternFunc, bt.ExternFunc);
    try t.eq(c.TypeType, bt.Type);
    try t.eq(c.TypeTccState, bt.TccState);
    try t.eq(c.TypeTuple, bt.Tuple);
    try t.eq(c.TypeFuncSig, bt.FuncSig);
    try t.eq(c.TypeExprType, bt.ExprType);
}

export fn clAsHostObject(val: Value) *anyopaque {
    return val.castHostObject(*anyopaque);
}

export fn clGetType(val: Value) c.Type {
    return val.getTypeId();
}

test "clGetTypeId()" {
    try t.eq(c.getType(c.float(123)), bt.Float);
}

export fn clIsFuture(vm: *cy.VM, val: Value) bool {
    return vm.c.types[val.getTypeId()].info.is_future;
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
    defer vm.destroy();

    var list: C.Value = undefined;
    vm.evalMust("List[dyn]{1, 2, 3}", &list);
    defer vm.release(list);

    // Initial cap.
    try t.eq(C.listLen(list), 3);
    try t.eq(C.listCap(list), 3);

    // Append.
    C.listAppend(vm, list, C.float(4));
    var res = C.listGet(vm, list, 3);
    try t.eq(C.asFloat(res), 4);
    try t.eq(C.listLen(list), 4);
    try t.eq(C.listCap(list), 12);

    // Get.
    res = C.listGet(vm, list, 1);
    try t.eq(C.asBoxInt(res), 2);

    // Set.
    C.listSet(vm, list, 1, C.float(100));
    res = C.listGet(vm, list, 1);
    try t.eq(C.asFloat(res), 100);

    // Insert.
    C.listInsert(vm, list, 0, C.float(123));
    res = C.listGet(vm, list, 0);
    try t.eq(C.asFloat(res), 123);
    try t.eq(C.listLen(list), 5);
}

export fn clGetFullVersion() C.Str {
    return C.toStr(build_options.full_version);
}

test "clGetFullVersion()" {
    const str = C.getFullVersion();
    try t.eqStr(C.fromStr(str), build_options.full_version);
}

export fn clGetVersion() C.Str {
    return C.toStr(build_options.version);
}

test "clGetVersion()" {
    const str = C.getVersion();
    try t.eqStr(C.fromStr(str), build_options.version);
}

export fn clGetBuild() C.Str {
    return C.toStr(build_options.build);
}

test "clGetBuild()" {
    const str = C.getBuild();
    try t.eqStr(C.fromStr(str), build_options.build);
}

export fn clGetCommit() C.Str {
    return C.toStr(build_options.commit);
}

test "clGetCommit()" {
    const str = C.getCommit();
    try t.eqStr(C.fromStr(str), build_options.commit);
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
        @export(&cy.compiler.defaultModuleResolver, .{ .name = "clDefaultResolver", .linkage = .strong });
        @export(&cy.compiler.defaultModuleLoader, .{ .name = "clDefaultModuleLoader", .linkage = .strong });
    }
}
