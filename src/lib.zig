const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = cy.fatal;
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const Value = cy.Value;
const zt = stdx.testing;
const log = cy.log.scoped(.lib);
const bt = cy.types.BuiltinTypes;
const C = @import("capi.zig");
const vmc = cy.vmc;

const cli = @import("cli.zig");

comptime {
    if (build_options.cli or builtin.os.tag == .wasi) {
        std.testing.refAllDecls(cli);
    }
}

export var clVerbose = false;
export var clSilent = false;

export fn cl_vm_init() *cy.VM {
    const alloc = cy.heap.getAllocator();
    const vm = alloc.create(cy.VM) catch fatal();
    vm.init(alloc) catch fatal();
    return @ptrCast(vm);
}

export fn cl_vm_deinit(vm: *cy.VM) void {
    vm.deinit(false);
    const alloc = cy.heap.getAllocator();
    alloc.destroy(vm);
}

export fn cl_thread_rc(t: *cy.Thread) usize {
    return t.getGlobalRC();
}

export fn cl_thread_count_objects(t: *cy.Thread) usize {
    return t.heap.count_objects();
}

export fn cl_thread_dump_live_objects(t: *cy.Thread) void {
    if (cy.Trace) {
        const w = std.debug.lockStderrWriter(&cy.debug.print_buf);
        defer std.debug.unlockStderrWriter();

        w.print("begin dump live objects\n", .{}) catch @panic("error");
        var iter = t.heap.objectTraceMap.iterator();
        while (iter.next()) |it| {
            const trace = it.value_ptr.*;
            if (trace.free_ctx == null) {
                cy.debug.write_object_alloc_trace(t, w, it.key_ptr.*, trace) catch fatal();
            }
        }
        w.print("end dump live objects\n", .{}) catch @panic("error");
    }
}

/// This is useful when calling into wasm to allocate some memory.
export fn clAlloc(vm: *cy.VM, size: usize) [*]const u8 {
    const slice = vm.alloc.alignedAlloc(u8, .@"8", size) catch fatal();
    return slice.ptr;
}

export fn cl_thread_ret(t: *cy.Thread, size: usize) *anyopaque {
    return t.ret_sized(size);
}

export fn cl_thread_param(t: *cy.Thread, size: usize) *anyopaque {
    return t.param_sized(size).ptr;
}

export fn cl_thread_float(t: *cy.Thread) f64 {
    return t.param(f64);
}

export fn cl_thread_ptr(t: *cy.Thread) *anyopaque {
    return t.param(*anyopaque);
}

export fn cl_new_bytes(vm: *cy.VM, size: usize) C.Bytes {
    const slice = vm.alloc.alignedAlloc(u8, .@"8", size) catch fatal();
    return C.to_bytes(slice);
}

export fn cl_vm_free(vm: *cy.VM, bytes: C.Bytes) void {
    vm.alloc.free(bytes.ptr[0..bytes.len]);
}

export fn cl_vm_freez(vm: *cy.VM, str: [*:0]const u8) void {
    vm.alloc.free(std.mem.sliceTo(str, 0));
}

export fn clGetAllocator(vm: *cy.VM) C.Allocator {
    return C.toAllocator(vm.alloc);
}

export fn clDefaultEvalConfig() C.EvalConfig {
    return .{
        .single_run = false,
        .file_modules = false,
        .reload = false,
        .backend = C.BackendVM,
        .gen_all_debug_syms = false,
        .spawn_exe = false, 
        .persist_main_locals = false,
    };
}

export fn clSetNewMockHttp(vm: *cy.VM) *anyopaque {
    const client = vm.alloc.create(cy.http.MockHttpClient) catch fatal();
    client.* = cy.http.MockHttpClient.init(vm.alloc);
    vm.httpClient = client.iface();
    return client;
}

export fn cl_vm_reset(vm: *cy.VM) void {
    vm.resetVM() catch cy.fatal();
}

export fn cl_vm_eval(vm: *cy.VM, src: C.Bytes, res: *C.EvalResult) C.ResultCode {
    const uri: []const u8 = "eval";
    return cl_vm_evalx(vm, C.to_bytes(uri), src, C.defaultEvalConfig(), res);
}

export fn cl_vm_evalx(vm: *cy.VM, uri: C.Bytes, src: C.Bytes, config: C.EvalConfig, out: *C.EvalResult) C.ResultCode {
    var res = C.Success;
    out.* = vm.eval(C.from_bytes(uri), C.from_bytes(src), config) catch |err| b: {
        switch (err) {
            error.CompileError => {
                res = C.ErrorCompile;
            },
            error.Panic => {
                res = C.ErrorPanic;
            },
            error.Await => {
                res = C.Await;
            },
            else => {
                log.tracev("{}", .{err});
                res = C.ErrorUnknown;
            },
        }
        break :b undefined;
    };
    vm.last_res = res;
    return res;
}

export fn cl_vm_eval_path(vm: *cy.VM, uri: C.Bytes, config: C.EvalConfig, out: *C.EvalResult) C.ResultCode {
    var res = C.Success;
    out.* = vm.eval(C.from_bytes(uri), null, config) catch |err| b: {
        switch (err) {
            error.CompileError => {
                res = C.ErrorCompile;
            },
            error.Panic => {
                res = C.ErrorPanic;
            },
            error.Await => {
                res = C.Await;
            },
            else => {
                log.tracev("{}", .{err});
                res = C.ErrorUnknown;
            },
        }
        break :b undefined;
    };
    vm.last_res = res;
    return res;
}

// export fn cl_run_ready_tasks(t: *cy.Thread) C.ResultCode {
//     var res = C.Success;
//     runReadyTasks(t) catch |err| {
//         switch (err) {
//             error.CompileError => {
//                 res = C.ErrorCompile;
//             },
//             error.Panic => {
//                 res = C.ErrorPanic;
//             },
//             error.Await => {
//                 res = C.Await;
//             },
//             else => {
//                 log.tracev("{}", .{err});
//                 res = C.ErrorUnknown;
//             },
//         }
//     };
//     t.last_res = res;
//     return res;
// }

// fn runReadyTasks(t: *cy.Thread) anyerror!void {
//     log.tracev("run ready tasks", .{});

//     while (t.ready_tasks.readItem()) |task| {
//         switch (task.type) {
//             .cont => {
//                 const cont = task.data.cont;
//                 t.c.stack = @ptrCast(cont.stack.ptr);
//                 t.c.stack_len = cont.stack.len;
//                 t.c.stackEndPtr = t.c.stack + t.c.stack_len;
//                 t.c.framePtr = t.c.stack + cont.fp;
//                 t.c.pc = cont.pc;
//                 @call(.never_inline, cy.Thread.evalLoopGrowStack, .{t.main_thread, true}) catch |err| {
//                     if (err == error.Await) {
//                         continue;
//                     } else {
//                         return err;
//                     }
//                 };

//                 // Reached end. Free task's stack.
//                 t.alloc.free(t.c.stack[0..t.c.stack_len]);
//                 t.c.stack_len = 0;
//             },
//             .callback => {
//                 // Create fiber.
//                 const initial_stack_size: u32 = 16;
//                 const stack = try t.heap.alloc.alloc(Value, initial_stack_size);
//                 t.c.stack = @ptrCast(stack.ptr);
//                 t.c.stack_len = stack.len;
//                 t.c.stackEndPtr = @ptrCast(t.c.stack + t.c.stack_len);

//                 // Indicate there is no previous fp.
//                 t.c.framePtr = t.c.stackEndPtr;
//                 _ = cy.vm.callUnion(t, task.data.callback, &.{}) catch |err| {
//                     if (err == error.Await) {
//                         return error.TODO;
//                     } else if (err == error.Panic) {
//                         try cy.vm.handleExecResult(t, vmc.RES_CODE_PANIC);
//                     } else {
//                         return err;
//                     }
//                 };

//                 // Release callback.
//                 t.heap.release(task.data.callback);

//                 // Reached end. Free task's stack.
//                 t.alloc.free(t.c.stack[0..t.c.stack_len]);
//             },
//             .dead => {},
//         }
//     }
// }

export fn clDefaultCompileConfig() C.CompileConfig {
    return .{
        .single_run = false,
        .gen_all_debug_syms = false,
        .file_modules = false,
        .backend = C.BackendVM,
        .skip_codegen = false,
        .emit_source_map = false,
        .persist_main_locals = false,
    };
}

export fn clCompile(vm: *cy.VM, uri: C.Bytes, src: C.Bytes, config: C.CompileConfig) C.ResultCode {
    var res = C.Success;
    _ = vm.compile(C.from_bytes(uri), C.from_bytes(src), config) catch |err| {
        switch (err) {
            error.CompileError => {
                res = C.ErrorCompile;
            },
            else => {
                res = C.ErrorUnknown;
            },
        }
    };
    vm.last_res = res;
    return res;
}

export fn clValidate(vm: *cy.VM, src: C.Bytes) C.ResultCode {
    var res = C.Success;
    _ = vm.validate("main", C.from_bytes(src), .{
        .file_modules = false,
    }) catch |err| {
        switch (err) {
            error.CompileError => {
                res = C.ErrorCompile;
            },
            else => {
                res = C.ErrorUnknown;
            },
        }
    };
    vm.last_res = res;
    return res;
}

test "clValidate()" {
    const vm = C.create();
    defer vm.destroy();

    C.setSilent(true);
    defer C.setSilent(false);

    var res = vm.validate("1 + 2");
    try zt.eq(res, C.Success);

    res = vm.validate("1 +");
    try zt.eq(res, C.ErrorCompile);
}

export fn cl_param_int(t: *cy.Thread, idx: usize) i64 {
    return t.getInt(@intCast(idx));
}

export fn cl_param_float(t: *cy.Thread, idx: usize) f64 {
    return t.getFloat(@intCast(idx));
}

export fn cl_param_value(t: *cy.Thread, idx: usize) Value {
    return t.getValue(@intCast(idx));
}

export fn clResultName(code: C.ResultCode) C.Bytes {
    switch (code) {
        C.Success => return C.to_bytes("Success"),
        C.ErrorCompile => return C.to_bytes("CompileError"),
        C.ErrorPanic => return C.to_bytes("PanicError"),
        C.ErrorUnknown => return C.to_bytes("UnknownError"),
        else => fatal(),
    }
}

var tempBuf: [1024]u8 align(8) = undefined;

export fn cl_vm_error_summary(vm: *cy.VM) C.Bytes {
    if (vm.last_res == C.ErrorCompile) {
        return cl_vm_compile_error_summary(vm);
    } else if (vm.last_res == C.ErrorPanic) {
        return cl_thread_panic_summary(vm.main_thread);
    } else {
        return C.NullStr;
    }
}

export fn cl_vm_compile_error_summary(vm: *cy.VM) C.Bytes {
    if (vm.compiler.reports.items.len == 0) {
        cy.panic("No report.");
    }
    const str = cy.debug.allocReportSummary(vm.compiler, vm.compiler.reports.items[0]) catch fatal();
    return C.to_bytes(str);
}

export fn cl_thread_panic_summary(t: *cy.Thread) C.Bytes {
    const summary = cy.debug.allocLastUserPanicError(t) catch fatal();
    return C.to_bytes(summary);
}

export fn clReportApiError(vm: *const cy.VM, msg: C.Bytes) void {
    vm.setApiError(C.from_bytes(msg)) catch fatal();
}

export fn cl_get_resolver(vm: *cy.VM) C.ResolverFn {
    return vm.compiler.moduleResolver;
}

export fn cl_set_resolver(vm: *cy.VM, resolver: C.ResolverFn) void {
    vm.compiler.moduleResolver = resolver;
}

export fn cl_resolve(vm: *cy.VM, uri: C.Bytes) C.Bytes {
    var buf: [4096]u8 = undefined;
    const r_uri_temp = cy.compiler.resolveModuleUri(vm.compiler, &buf, C.from_bytes(uri)) catch fatal();
    const r_uri = vm.alloc.dupe(u8, r_uri_temp) catch fatal();
    return C.to_bytes(r_uri);
}

export fn cl_vm_get_loader(vm: *cy.VM) C.ModuleLoaderFn {
    return vm.compiler.moduleLoader;
}

export fn cl_vm_set_loader(vm: *cy.VM, loader: C.ModuleLoaderFn) void {
    vm.compiler.moduleLoader = loader;
}

export fn cl_vm_printer(vm: *cy.VM) C.PrintFn {
    return vm.print;
}

export fn cl_vm_set_printer(vm: *cy.VM, print: C.PrintFn) void {
    vm.print = print;
}

export fn cl_vm_eprinter(vm: *cy.VM) C.PrintErrorFn {
    return vm.print_err;
}

export fn cl_vm_set_eprinter(vm: *cy.VM, print: C.PrintErrorFn) void {
    vm.print_err = print;
}

export fn cl_vm_set_logger(vm: *cy.VM, logger: C.LogFn) void {
    vm.log = logger;
}

export fn cl_thread_detect_cycles(t: *cy.Thread) C.GCResult {
    const res = t.detect_cycles() catch cy.fatal();
    return .{
        .num_obj_freed = res.num_obj_freed,
    };
}

export fn clFindType(vm: *cy.VM, spec: C.Bytes) ?*cy.Type {
    return vm.findType(C.from_bytes(spec)) catch @panic("error");
}

test "clFindType()" {
    const vm = C.create();
    defer vm.destroy();

    // Before types are loaded.
    try zt.eq(vm.findType("int"), null);

    // Load builtin type.
    _ = vm.compile("main", "", C.defaultCompileConfig());
    try zt.eq(vm.findType("int").?.id(), C.TypeInteger);

    // Load type declared from main.
    _ = vm.compile("main",
        \\type Foo:
        \\    a int
    , C.defaultCompileConfig());
    try zt.expect(vm.findType("Foo") != null);
}

export fn clExpandTypeTemplate(vm: *cy.VM, ctemplate: ?*C.Sym, types_ptr: [*]const *cy.Type, args_ptr: [*]const cy.Value, nargs: usize) ?*cy.Type {
    const template = cy.Sym.fromC(ctemplate).cast(.template);
    const args = args_ptr[0..nargs];
    const param_types = types_ptr[0..nargs];
    return vm.expandTypeTemplate(template, param_types, args) catch @panic("error");
}

export fn cl_mod_add_type(mod: *C.Sym, name: C.Bytes, binding: C.BindType) void {
    const c: *cy.sym.Chunk = @ptrCast(@alignCast(mod));
    c.chunk.host_types.put(c.chunk.alloc, C.from_bytes(name), binding) catch @panic("error");
}

export fn cl_mod_add_func(mod: *C.Sym, name: C.Bytes, binding: C.BindFunc) void {
    const c: *cy.sym.Chunk = @ptrCast(@alignCast(mod));
    c.chunk.host_funcs.put(c.chunk.alloc, C.from_bytes(name), binding) catch @panic("error");
}

export fn cl_mod_add_global(mod: *C.Sym, name: C.Bytes, binding: C.BindGlobal) void {
    const c: *cy.sym.Chunk = @ptrCast(@alignCast(mod));
    c.chunk.host_globals.put(c.chunk.alloc, C.from_bytes(name), binding) catch @panic("error");
}

export fn cl_mod_on_destroy(mod: *C.Sym, on_destroy: C.ModuleOnDestroyFn) void {
    const c: *cy.sym.Chunk = @ptrCast(@alignCast(mod));
    c.chunk.on_destroy = on_destroy;
}

export fn cl_mod_on_load(mod: *C.Sym, on_load: C.ModuleOnLoadFn) void {
    const c: *cy.sym.Chunk = @ptrCast(@alignCast(mod));
    c.chunk.on_load = on_load;
}

export fn cl_thread_release(t: *cy.Thread, val: Value) void {
    t.heap.release(val);
}

export fn cl_thread_retain(t: *cy.Thread, val: Value) void {
    t.heap.retain(val);
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

export fn cl_float(n: f64) Value {
    return Value.initFloat64(n);
}

export fn cl_int(n: i64) Value {
    return Value.initInt(@intCast(n));
}

export fn clInt32(n: i32) Value {
    return Value.initInt(n);
}

export fn clPtr(ptr: *anyopaque) Value {
    return Value.initPtr(ptr);
}

export fn cl_str_init(t: *cy.Thread, cstr: C.Bytes) C.str {
    return @bitCast(t.heap.init_str(C.from_bytes(cstr)) catch fatal());
}

export fn cl_astr_init(t: *cy.Thread, cstr: C.Bytes) C.str {
    return @bitCast(t.heap.init_astr(C.from_bytes(cstr)) catch fatal());
}

export fn cl_ustr_init(t: *cy.Thread, cstr: C.Bytes) C.str {
    return @bitCast(t.heap.init_ustr(C.from_bytes(cstr)) catch fatal());
}

export fn cl_str_deinit(t: *cy.Thread, s: *cy.heap.Str) void {
    t.heap.destructStr(s);
}

export fn cl_array_empty(t: *cy.Thread, array_t: *cy.Type) Value {
    return t.heap.newEmptyArray(array_t.id()) catch fatal();
}

export fn cl_map_empty(t: *cy.Thread, map_t: *cy.Type) Value {
    return t.heap.allocMapZero(map_t.id()) catch fatal();
}

export fn cl_new_func_union(t: *cy.Thread, union_t: *cy.Type, func: C.HostFn) Value {
    return t.heap.allocHostFuncUnion(union_t.id(), @ptrCast(func)) catch fatal();
}

export fn clGetFuncSig(vm: *cy.VM, params: [*]const *cy.Type, nparams: usize, ret_t: *cy.Type) *cy.FuncSig {
    return vm.sema.ensureFuncSig(@ptrCast(params[0..nparams]), ret_t) catch fatal();
}

// test "clNewFunc()" {
//     const vm = C.create();
//     defer vm.destroy();

//     const val = vm.newFunc(&.{}, bt.Dyn, @ptrFromInt(8));
//     try t.eq(C.getType(val), bt.Func);
// }

export fn cl_mod_set_data(mod_: *C.Sym, data: ?*anyopaque) void {
    const sym: *cy.sym.Chunk = @ptrCast(@alignCast(mod_));
    const chunk = sym.chunk;
    chunk.user_data = data;
}

export fn cl_mod_get_data(mod_: *C.Sym) ?*anyopaque {
    const sym: *cy.sym.Chunk = @ptrCast(@alignCast(mod_));
    const chunk = sym.chunk;
    return chunk.user_data;
}

export fn cl_lift(t: *cy.Thread, type_: *cy.Type, value: *anyopaque) Value {
    const res = t.heap.new_object_undef(type_.id(), type_.size()) catch @panic("error");
    const dst: [*]u8 = @ptrCast(res);
    const src: [*]u8 = @ptrCast(value);
    @memcpy(dst[0..type_.size()], src[0..type_.size()]);
    return Value.initPtr(res);
}

test "cl_struct_new()" {
    const vm = C.create();
    defer vm.destroy();

    var res: C.Value = undefined;
    vm.evalMust( 
        \\type Foo:
        \\    a int
        \\    b string
    , &res);
    const foo_t = vm.findType("Foo").?;
    const val = vm.newInstance(foo_t, &.{
        C.toFieldInit("a", C.int(123)),
        C.toFieldInit("b", vm.newString("abc")),
    });
    // try t.eq(C.getType(val), foo_t);

    try zt.eq(vm.getField(val, "a"), 123);
    try zt.eqStr(C.asString(vm.getField(val, "b")), "abc");
}

export fn cl_option_none_new(t: *cy.Thread, option_t: *cy.Type) cy.Value {
    return t.heap.newNone(option_t.cast(.option)) catch @panic("error");
}

export fn cl_option_new(t: *cy.Thread, option_t: *cy.Type, val: cy.Value) cy.Value {
    return t.heap.newSome(option_t.cast(.option), val) catch @panic("error");
}

export fn cl_result_new(t: *cy.Thread, res_t: *cy.Type, val: cy.Value) cy.Value {
    return t.heap.newRes(res_t.cast(.result), val) catch @panic("error");
}

export fn cl_result_error_new(t: *cy.Thread, res_t: *cy.Type, err: cy.Value) cy.Value {
    return t.heap.newErr(res_t.cast(.result), err) catch @panic("error");
}

export fn cl_choice_new(t: *cy.Thread, choice_t: *cy.Type, name: C.Bytes, val: cy.Value) cy.Value {
    return t.heap.newChoice(choice_t, C.from_bytes(name), val) catch @panic("error");
}

test "cl_choice_new()" {
    const vm = C.create();
    defer vm.destroy();

    var res: C.Value = undefined;
    vm.evalMust( 
        \\type Foo enum:
        \\    case a int
        \\    case b string
    , &res);
    const foo_t = vm.findType("Foo").?;
    const val = vm.newChoice(foo_t, "a", C.int(123));
    // try t.eq(C.getType(val), foo_t);
    try zt.eq(vm.unwrapChoice(foo_t, val, "a"), 123);
}

export fn clSymbol(vm: *cy.VM, str: C.Bytes) Value {
    const id = vm.sema.ensureSymbol(C.from_bytes(str)) catch fatal();
    return Value.initSymbol(@intCast(id));
}

export fn clGetField(vm: *cy.VM, val: cy.Value, name: C.Bytes) cy.Value {
    _ = vm;
    _ = val;
    _ = name;
    // return vm.getFieldName(val, C.fromStr(name)) catch {
    //     return cy.panicFmt("Can not access field: `{s}`", .{C.fromStr(name)});
    // };
    @panic("TODO");
}

test "clGetField()" {
    const vm = C.create();
    defer vm.destroy();

    var res: C.Value = undefined;
    vm.evalMust( 
        \\type Foo:
        \\    a int
        \\    b string
        \\Foo{a=123, b='abc'}
    , &res);
    defer vm.release(res);
    try zt.eq(vm.getField(res, "a"), 123);
    try zt.eqStr(C.asString(vm.getField(res, "b")), "abc");
}

export fn cl_choice_unwrap(t: *cy.Thread, choice_t: *cy.Type, choice: cy.Value, name: C.Bytes) cy.Value {
    const zname = C.from_bytes(name);
    return t.heap.unwrapChoice(choice_t, choice, zname) catch |err| {
        return cy.panicFmt("{}", .{err});
    };
}

// To enable logging for tests:
// c.setVerbose(true);
// c.setLog(printLogger);
fn printLogger(str: C.Bytes) callconv(.C) void {
    std.debug.print("{s}\n", .{ C.from_bytes(str) });
}

test "clUnwrapChoice()" {
    const vm = C.create();
    defer vm.destroy();

    var res: C.Value = undefined;
    vm.evalMust( 
        \\type Foo enum:
        \\    case a int
        \\    case b string
        \\Foo.a(123)
    , &res);
    defer vm.release(res);

    const choice_t = C.getType(res);
    const unboxed = C.unboxPtr(res);
    try zt.eq(vm.unwrapChoice(choice_t, unboxed, "a"), 123);
}

export fn cl_as_float(val: Value) f64 {
    return val.asF64();
}

test "cl_as_float()" {
    try zt.eq(C.asFloat(C.float(123.0)), 123);
}

export fn clRefToBool(val: Value) bool {
    return val.refToBool();
}

test "clToBool()" {
    const vm = C.create();
    defer vm.destroy();

    try zt.eq(C.toBool(C.float(0)), false);
    try zt.eq(C.toBool(C.float(123.0)), true);

    var i = vm.newInt(0);
    try zt.eq(C.toBool(i), false);
    i = vm.newInt(1);
    try zt.eq(C.toBool(i), true);

    try zt.eq(C.toBool(C.True), true);
    try zt.eq(C.toBool(C.False), false);
}

export fn clAsBool(val: Value) bool {
    return val.asBool();
}

export fn clAsRefBool(val: Value) bool {
    return val.asRefBool();
}

test "clAsBoxInt()" {
    const vm = C.create();
    defer vm.destroy();

    const val = vm.newInt(123);
    try zt.eq(C.asBoxInt(val), 123);
}

export fn clAsString(val: cy.Value) C.Bytes {
    return C.to_bytes(val.asString());
}

export fn clAsSymbol(val: Value) u64 {
    return val.asSymbol();
}

test "clAsSymbol()" {
    const vm = C.create();
    defer vm.destroy();

    var val = C.symbol(vm, "foo");
    try zt.eq(C.asSymbol(val), 49);

    val = C.symbol(vm, "bar");
    try zt.eq(C.asSymbol(val), 50);

    val = C.symbol(vm, "foo");
    try zt.eq(C.asSymbol(val), 49);
}

export fn cl_object_string(t: *cy.Thread, val: Value) C.Bytes {
    const str = t.heap.get_or_buf_print_object(&cy.tempBuf, val) catch cy.fatal();
    return C.to_bytes(str);
}

test "Constants." {
    try zt.eq(C.TypeVoid, bt.Void);
    try zt.eq(C.TypeBoolean, bt.Boolean);
    try zt.eq(C.TypeError, bt.Error);
    try zt.eq(C.TypeSymbol, bt.Symbol);
    try zt.eq(C.TypeInteger, bt.Integer);
    try zt.eq(C.TypeFloat, bt.Float);
    try zt.eq(C.TypeFunc, bt.Func);
    try zt.eq(C.TypeString, bt.String);
    try zt.eq(C.TypeFiber, bt.Fiber);
    try zt.eq(C.TypeExternFunc, bt.ExternFunc);
    try zt.eq(C.TypeType, bt.Type);
    try zt.eq(C.TypeTccState, bt.TccState);
    try zt.eq(C.TypeFuncSig, bt.FuncSig);
    try zt.eq(C.TypeCode, bt.Code);
}

export fn clTypeId(type_: *cy.Type) cy.TypeId {
    return type_.id();
}

export fn clGetType(vm: *cy.VM, val: Value) *C.Type {
    return @ptrCast(vm.sema.getType(@intCast(val.asInt())));
}

// test "clGetTypeId()" {
//     try t.eq(C.getType(C.float(123)), bt.Float);
// }

export fn cl_vm_main_thread(vm: *cy.VM) *cy.Thread {
    return vm.main_thread;
}

export fn cl_value_desc(vm: *cy.VM, val_t: cy.TypeId, val: *Value) C.Bytes {
    const res = cy.debug.alloc_value_desc(vm, val_t, val) catch @panic("error");
    return C.to_bytes(res);
}

export fn clGetFullVersion() C.Bytes {
    return C.to_bytes(build_options.full_version);
}

test "clGetFullVersion()" {
    const str = C.getFullVersion();
    try zt.eqStr(C.from_bytes(str), build_options.full_version);
}

export fn clGetVersion() C.Bytes {
    return C.to_bytes(build_options.version);
}

test "clGetVersion()" {
    const str = C.getVersion();
    try zt.eqStr(C.from_bytes(str), build_options.version);
}

export fn clGetBuild() C.Bytes {
    return C.to_bytes(build_options.build);
}

test "clGetBuild()" {
    const str = C.getBuild();
    try zt.eqStr(C.from_bytes(str), build_options.build);
}

export fn clGetCommit() C.Bytes {
    return C.to_bytes(build_options.commit);
}

test "clGetCommit()" {
    const str = C.getCommit();
    try zt.eqStr(C.from_bytes(str), build_options.commit);
}

/// Used in C/C++ code to log synchronously.
pub export fn zig_log(buf: [*c]const u8) void {
    log.tracev("{s}", .{ buf });
}

pub export fn zig_log_u32(buf: [*c]const u8, val: u32) void {
    log.tracev("{s}: {}", .{ buf, val });
}

comptime {
    @export(&cy.compiler.defaultModuleResolver, .{ .name = "cl_default_resolver", .linkage = .strong });
    @export(&cy.compiler.defaultModuleLoader, .{ .name = "cl_default_loader", .linkage = .strong });
}
