const std = @import("std");
const build_config = @import("build_config");
const builtin = @import("builtin");
const stdx = @import("stdx");
const fatal = cy.fatal;
const mi = @import("mimalloc");
const cy = @import("cyber.zig");
const meta_mod = @import("builtins/meta.zig");
const Value = cy.Value;
const zt = stdx.testing;
const log = cy.log.scoped(.lib);
const bt = cy.types.BuiltinTypes;
const C = @import("capi.zig");
const is_wasm = builtin.cpu.arch.isWasm();
const vmc = cy.vmc;

export var clVerbose = false;
export var clSilent = false;

export var cl_logger: C.GlobalLogFn = default_logger;
export var CL_TRACE: bool = build_config.trace;

fn default_logger(s: C.Bytes) callconv(.c) void {
    _ = s;
}

export fn cl_vm_init() *cy.VM {
    const alloc = std.heap.c_allocator;
    const vm = alloc.create(cy.VM) catch fatal();
    vm.init(alloc) catch fatal();
    return @ptrCast(vm);
}

export fn cl_vm_initx(allocator: C.Allocator) *cy.VM {
    const alloc = C.fromAllocator(allocator);
    const vm = alloc.create(cy.VM) catch fatal();
    vm.init(alloc) catch fatal();
    return @ptrCast(vm);
}

export fn cl_vm_deinit(vm: *cy.VM) void {
    vm.deinit(false);
    vm.alloc.destroy(vm); 
}

export fn cl_thread_vm(t: *cy.Thread) *cy.VM {
    return t.c.vm;
}

export fn cl_thread_allocator(t: *cy.Thread) C.Allocator {
    return C.toAllocator(t.alloc);
}

export fn cl_thread_dump_stats(t: *cy.Thread) void {
    return t.dump_stats() catch @panic("error");
}

export fn cl_vm_allocator(vm: *cy.VM) C.Allocator {
    return C.toAllocator(vm.alloc);
}

export fn cl_thread_rc(t: *cy.Thread) usize {
    return t.getGlobalRC();
}

export fn cl_thread_count_objects(t: *cy.Thread) usize {
    return t.heap.count_objects();
}

export fn cl_thread_dump_live_objects(t: *cy.Thread) void {
    if (cy.Trace) {
        var lw = std.Io.Writer.Allocating.init(t.alloc);
        defer lw.deinit();
        const w = &lw.writer;
        w.print("begin dump live objects\n", .{}) catch @panic("error");
        var iter = t.heap.objectTraceMap.iterator();
        while (iter.next()) |it| {
            const trace = it.value_ptr.*;
            if (trace.free_ctx == null) {
                cy.debug.write_object_alloc_trace(t, w, it.key_ptr.*, trace) catch fatal();
            }
        }
        w.print("end dump live objects\n", .{}) catch @panic("error");
        t.c.vm.log(lw.written());
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

export fn cl_thread_ret_panic(t: *cy.Thread, msg: C.Bytes) C.Ret {
    return t.ret_panic(C.from_bytes(msg));
}

export fn cl_thread_param(t: *cy.Thread, size: usize) *anyopaque {
    return t.param_sized(size).ptr;
}

export fn cl_thread_float(t: *cy.Thread) f64 {
    return t.param(f64);
}

export fn cl_thread_f32(t: *cy.Thread) f32 {
    return t.param(f32);
}

export fn cl_thread_int(t: *cy.Thread) i64 {
    return t.param(i64);
}

export fn cl_thread_i32(t: *cy.Thread) i32 {
    return t.param(i32);
}

export fn cl_thread_i16(t: *cy.Thread) i16 {
    return t.param(i16);
}

export fn cl_thread_i8(t: *cy.Thread) i8 {
    return t.param(i8);
}

export fn cl_thread_r64(t: *cy.Thread) u64 {
    return t.param(u64);
}

export fn cl_thread_r32(t: *cy.Thread) u32 {
    return t.param(u32);
}

export fn cl_thread_r16(t: *cy.Thread) u16 {
    return t.param(u16);
}

export fn cl_thread_byte(t: *cy.Thread) u8 {
    return t.param(u8);
}

export fn cl_thread_slice(t: *cy.Thread) cy.heap.Slice {
    return t.param(cy.heap.Slice);
}

export fn cl_thread_str(t: *cy.Thread) cy.heap.Str {
    return t.param(cy.heap.Str);
}

export fn cl_thread_ptr(t: *cy.Thread) *anyopaque {
    return t.param(*anyopaque);
}

export fn cl_slice_init(t: *cy.Thread, num_elems: usize, elem_size: usize) cy.heap.Slice {
    return t.heap.init_slice_undef(t.c.vm.sema.raw_buffer_byte_t.id(), num_elems, elem_size) catch fatal();
}

export fn cl_vm_alloc(vm: *cy.VM, size: usize) *anyopaque {
    const slice = vm.alloc.alignedAlloc(u8, .@"8", size) catch fatal();
    return slice.ptr;
}

export fn cl_vm_free(vm: *cy.VM, ptr: *anyopaque, size: usize) void {
    vm.alloc.free(@as([*]align(8) u8, @ptrCast(@alignCast(ptr)))[0..size]);
}

export fn cl_vm_allocb(vm: *cy.VM, size: usize) C.Bytes {
    const slice = vm.alloc.alloc(u8, size) catch fatal();
    return C.to_bytes(slice);
}

export fn cl_vm_freeb(vm: *cy.VM, bytes: C.Bytes) void {
    vm.alloc.free(C.from_bytes(bytes));
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
        .backend = C.BackendVM,
        .gen_all_debug_syms = false,
        .spawn_exe = false, 
        .persist_main = false,
    };
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
        .backend = C.BackendVM,
        .skip_codegen = false,
        .emit_source_map = false,
        .persist_main = false,
    };
}

export fn cl_vm_compile_path(vm: *cy.VM, uri: C.Bytes, config: C.CompileConfig) C.ResultCode {
    var res = C.Success;
    _ = vm.compile(C.from_bytes(uri), null, config) catch |err| {
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

export fn cl_vm_compile(vm: *cy.VM, uri: C.Bytes, src: C.Bytes, config: C.CompileConfig) C.ResultCode {
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

export fn cl_vm_validate(vm: *cy.VM, src: C.Bytes) C.ResultCode {
    var res = C.Success;
    _ = vm.validate("main", C.from_bytes(src), .{}) catch |err| {
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

test "cl_vm_validate()" {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    C.setSilent(true);
    defer C.setSilent(false);

    var res = C.vm_validate(vm, "_ = 1 + 2");
    try zt.eq(res, C.Success);

    res = C.vm_validate(vm, "_ = 1 +");
    try zt.eq(res, C.ErrorCompile);
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

export fn cl_thread_panic_trace(t: *cy.Thread) C.StackTrace {
    return .{
        .frames = @ptrCast(t.stack_trace.frames.ptr),
        .frames_len = t.stack_trace.frames.len,
    };
}

export fn clReportApiError(vm: *const cy.VM, msg: C.Bytes) void {
    vm.setApiError(C.from_bytes(msg)) catch fatal();
}

export fn cl_vm_resolver(vm: *cy.VM) C.ResolverFn {
    return vm.compiler.resolver;
}

export fn cl_vm_set_resolver(vm: *cy.VM, resolver: C.ResolverFn) void {
    vm.compiler.resolver = resolver;
}

export fn cl_vm_resolve(vm: *cy.VM, uri: C.Bytes) C.Bytes {
    var buf: [4096]u8 = undefined;
    const r_uri_temp = cy.compiler.resolveModuleUri(vm.compiler, &buf, C.from_bytes(uri)) catch fatal();
    const r_uri = vm.alloc.dupe(u8, r_uri_temp) catch fatal();
    return C.to_bytes(r_uri);
}

export fn cl_vm_set_dl_resolver(vm: *cy.VM, dl_resolver: C.DlResolverFn) void {
    vm.compiler.dl_resolver = dl_resolver;
}

export fn cl_vm_get_loader(vm: *cy.VM) C.ModuleLoaderFn {
    return vm.compiler.loader;
}

export fn cl_vm_set_loader(vm: *cy.VM, loader: C.ModuleLoaderFn) void {
    vm.compiler.loader = loader;
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

export fn cl_vm_logger(vm: *cy.VM) C.LogFn {
    return vm.log_fn;
}

export fn cl_vm_set_logger(vm: *cy.VM, logger: C.LogFn) void {
    vm.log_fn = logger;
}

export fn cl_vm_dump_bytecode(vm: *cy.VM) void {
    cy.debug.dumpBytecode(vm, .{}) catch @panic("error");
}

export fn cl_thread_check_memory(t: *cy.Thread) C.MemoryCheck {
    return t.check_memory() catch cy.fatal();
}

export fn cl_find_type(vm: *cy.VM, spec: C.Bytes) ?*cy.Type {
    return vm.findType(C.from_bytes(spec)) catch @panic("error");
}

test "cl_find_type()" {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    // Before types are loaded.
    try zt.eq(C.vm_find_type(vm, "int"), null);

    // Load builtin type.
    _ = C.vm_compile(vm, "main", "", C.defaultCompileConfig());
    const act: *cy.Type = @ptrCast(@alignCast(C.vm_find_type(vm, "int")));
    try zt.eq(act.id(), C.TypeI64);

    // Load type declared from main.
    _ = C.vm_compile(vm, "main",
        \\type Foo:
        \\    a int
    , C.defaultCompileConfig());
    try zt.expect(C.vm_find_type(vm, "Foo") != null);
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

export fn cl_vm_user_data(vm: *cy.VM) ?*anyopaque {
    return vm.userData;
}

export fn cl_vm_set_user_data(vm: *cy.VM, userData: ?*anyopaque) void {
    vm.userData = userData;
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

export fn cl_str_bytes(s: cy.heap.Str) C.Bytes {
    return C.to_bytes(s.slice());
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

export fn cl_vm_ensure_func_sig(vm: *cy.VM, params: [*]const *cy.Type, nparams: usize, ret_t: *cy.Type) *cy.FuncSig {
    return vm.sema.ensureFuncSig(@ptrCast(@alignCast(params[0..nparams])), ret_t) catch fatal();
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

// To enable logging for tests:
// c.setVerbose(true);
// c.setLog(printLogger);
fn printLogger(str: C.Bytes) callconv(.C) void {
    std.debug.print("{s}\n", .{ C.from_bytes(str) });
}

export fn cl_symbol(vm: *cy.VM, name: C.Bytes) u64 {
    return vm.sema.ensureSymbol(C.from_bytes(name)) catch @panic("error");
}

test "cl_symbol()" {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    const val = C.symbol(vm, "foo");
    try zt.eq(val + 1, C.symbol(vm, "foo2"));
    try zt.eq(val, C.symbol(vm, "foo"));
}

export fn cl_object_string(t: *cy.Thread, val: Value) C.Bytes {
    const str = t.heap.get_or_buf_print_object(&cy.tempBuf, val) catch cy.fatal();
    return C.to_bytes(str);
}

test "Constants." {
    try zt.eq(C.TypeVoid, bt.Void);
    try zt.eq(C.TypeBool, bt.Bool);
    try zt.eq(C.TypeError, bt.Error);
    try zt.eq(C.TypeSymbol, bt.Symbol);
    try zt.eq(C.TypeI64, bt.I64);
    try zt.eq(C.TypeF64, bt.F64);
    try zt.eq(C.TypeStr, bt.Str);
    // try zt.eq(C.TypeFiber, bt.Fiber);
    try zt.eq(C.TypeType, bt.Type);
    try zt.eq(C.TypeFuncSig, bt.FuncSig);
    try zt.eq(C.TypeCode, bt.Code);
}

export fn cl_type_id(type_: *cy.Type) cy.TypeId {
    return type_.id();
}

export fn cl_type_from_id(vm: *cy.VM, type_id: cy.TypeId) *C.Type {
    return @ptrCast(vm.sema.getType(type_id));
}

// test "clGetTypeId()" {
//     try t.eq(C.getType(C.float(123)), bt.Float);
// }

export fn cl_vm_main_thread(vm: *cy.VM) *cy.Thread {
    return vm.main_thread;
}

export fn cl_thread_trace_info(t: *cy.Thread) C.TraceInfo {
    return .{
        .num_retains = t.heap.c.numRetains,
        .num_releases = t.heap.c.numReleases,
    };
}

export fn cl_thread_signal_host_segfault(t: *cy.Thread) void {
    cy.debug.thread_signal_host_segfault(t) catch @panic("error");
}

export fn cl_thread_signal_host_panic(t: *cy.Thread) void {
    cy.debug.thread_signal_host_panic(t) catch @panic("error");
}

export fn cl_value_desc(vm: *cy.VM, val_t: cy.TypeId, val: *Value) C.Bytes {
    const res = cy.debug.alloc_value_desc(vm, val_t, val) catch @panic("error");
    return C.to_bytes(res);
}

export fn cl_full_version() C.Bytes {
    return C.to_bytes(build_config.full_version);
}

test "cl_full_version()" {
    const str = C.full_version();
    try zt.eqStr(str, build_config.full_version);
}

export fn cl_version() C.Bytes {
    return C.to_bytes(build_config.version);
}

test "cl_version()" {
    const str = C.version();
    try zt.eqStr(str, build_config.version);
}

export fn cl_build() C.Bytes {
    return C.to_bytes(build_config.build);
}

test "cl_build()" {
    const str = C.build();
    try zt.eqStr(str, build_config.build);
}

export fn cl_commit() C.Bytes {
    return C.to_bytes(build_config.commit);
}

test "cl_commit()" {
    const str = C.commit();
    try zt.eqStr(str, build_config.commit);
}

comptime {
    @export(&cy.compiler.default_resolver, .{ .name = "cl_default_resolver", .linkage = .strong });
    @export(&cy.compiler.default_loader, .{ .name = "cl_default_loader", .linkage = .strong });
}

fn test_logger(_: ?*C.VM, str: C.Bytes) callconv(.c) void {
    std.debug.print("{s}\n", .{C.from_bytes(str)});
}

fn test_global_logger(str: C.Bytes) callconv(.c) void {
    std.debug.print("{s}\n", .{C.from_bytes(str)});
}

fn debug(vm: ?*C.VM) void {
    C.setVerbose(true);
    C.vm_set_logger(vm, &test_logger);
    C.set_logger(&test_global_logger);
}

test "Return from main." {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    var res: C.EvalResult = undefined;
    _ = C.vm_eval(vm,
        \\fn main() -> int:
        \\  return 123
    , &res);
    try zt.eq(123, @as(*i64, @ptrCast(res.res)).*);
}

test "cl_slice_init." {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    const S = struct {
        fn mod_test(t_: *C.Thread) callconv(.c) C.Ret {
            const ret: *C.Slice = C.thread_ret(t_, C.Slice);
            ret.* = C.slice_init(t_, 2, 8);
            var elems: [*]i64 = @ptrCast(@alignCast(ret.ptr));
            elems[0] = 123;
            elems[1] = 234;
            return C.RetOk;
        }
        fn loader(vm_: *C.VM, mod: *C.Sym, uri: C.Bytes, res: [*c]C.LoaderResult) callconv(.c) bool {
            const name = C.from_bytes(uri);
            if (std.mem.eql(u8, name, "mod")) {
                C.mod_add_func(mod, "test", C.BIND_FUNC(&mod_test));
                res[0].src = C.to_bytes("#[bind] fn test() -> []int");
                return true;
            } else {
                return C.default_loader(vm_, mod, uri, res);
            }
        }
    };
    C.vm_set_loader(vm, @ptrCast(&S.loader));

    var res: C.EvalResult = undefined;
    const code = C.vm_eval(vm, 
        \\use mod
        \\use test
        \\slice := mod.test()
        \\test.eq(2, slice.len())
        \\test.eq(123, slice[0])
        \\test.eq(234, slice[1])
    , &res);
    try zt.eq(C.Success, code);
}

test "Bind functions." {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    const S = struct {
        fn mod_test(t_: *C.Thread) callconv(.c) C.Ret {
            const ret: *i64 = C.thread_ret(t_, i64);
            const a = C.thread_int(t_);
            ret.* = a + 1;
            return C.RetOk;
        }
        fn loader(vm_: *C.VM, mod: *C.Sym, uri: C.Bytes, res: [*c]C.LoaderResult) callconv(.c) bool {
            const name = C.from_bytes(uri);
            if (std.mem.eql(u8, name, "mod")) {
                C.mod_add_func(mod, "test", C.BIND_FUNC(&mod_test));
                res[0].src = C.to_bytes("#[bind] fn test(a int) -> int");
                return true;
            } else {
                return C.default_loader(vm_, mod, uri, res);
            }
        }
    };
    C.vm_set_loader(vm, @ptrCast(&S.loader));

    var res: C.EvalResult = undefined;
    _ = C.vm_eval(vm, 
        \\use mod
        \\use test
        \\test.eq(123, mod.test(122))
    , &res);
}

test "Stack trace unwinding." {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    var res: C.EvalResult = undefined;
    var code = C.vm_eval(vm,
        \\use test
        \\a := 123
        \\b := a / 0
    , &res);

    try zt.eq(C.ErrorPanic, code);
    var thread = C.vm_main_thread(vm);
    {
        const report = C.thread_panic_summary(thread);
        defer C.vm_freeb(vm, report);
        try zt.eqStr(
            \\panic: Division by zero.
            \\
            \\eval:3:6 main:
            \\b := a / 0
            \\     ^
            \\
        , report);
    }

    var trace = C.thread_panic_trace(thread);
    try zt.eq(trace.frames_len, 1);
    try eqStackFrame(trace.frames[0], .{
        .name = C.to_bytes("main"),
        .chunk = 0,
        .line = 2,
        .col = 5,
        .line_pos = 18,
    });

    C.vm_reset(vm);

    // Function stack trace.
    code = C.vm_eval(vm, 
        \\use test
        \\fn foo() -> int:
        \\  a := 123
        \\  return a / 0
        \\b := foo()
    , &res);

    try zt.eq(C.ErrorPanic, code);
    thread = C.vm_main_thread(vm);
    {
        const report = C.thread_panic_summary(thread);
        defer C.vm_freeb(vm, report);
        try zt.eqStr(
            \\panic: Division by zero.
            \\
            \\eval:4:10 foo:
            \\  return a / 0
            \\         ^
            \\eval:5:6 main:
            \\b := foo()
            \\     ^
            \\
        , report);
    }
    trace = C.thread_panic_trace(thread);
    try zt.eq(trace.frames_len, 2);
    try eqStackFrame(trace.frames[0], .{
        .name = C.to_bytes("foo"),
        .chunk = 0,
        .line = 3,
        .col = 9,
        .line_pos = 37,
    });
    try eqStackFrame(trace.frames[1], .{
        .name = C.to_bytes("main"),
        .chunk = 0,
        .line = 4,
        .col = 5,
        .line_pos = 52,
    });
}

fn eqStackFrame(act: C.StackFrame, exp: C.StackFrame) !void {
    try zt.eqStr(C.from_bytes(act.name), C.from_bytes(exp.name));
    try zt.eq(exp.chunk, act.chunk);
    try zt.eq(exp.line, act.line);
    try zt.eq(exp.col, act.col);
    try zt.eq(exp.line_pos, act.line_pos);
}

test "Bind global persists across multiple evals." {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    var global: i64 = 0;
    C.vm_set_user_data(vm, &global);
    C.vm_set_loader(vm, struct {
        fn loader(vm_: ?*C.VM, mod: ?*C.Sym, spec: C.Bytes, res: [*c]C.LoaderResult) callconv(.c) bool {
            if (std.mem.eql(u8, C.from_bytes(spec), "mod")) {
                const ptr = C.vm_user_data(vm_);
                C.mod_add_global(mod.?, "g", C.BIND_GLOBAL(ptr));
                res[0].src = C.to_bytes("#[bind] global g int");
                return true;
            } else {
                return C.default_loader(vm_, mod, spec, res);
            }
        }
    }.loader);

    try zt.eq(0, global);
    var eval_res: C.EvalResult = undefined;
    _ = C.vm_eval(vm, 
        \\use m 'mod'
        \\m.g = 1
    , &eval_res);
    try zt.eq(1, global);
    _ = C.vm_eval(vm, 
        \\use m 'mod'
        \\m.g = m.g + 1
    , &eval_res);
    try zt.eq(2, global);
}

test "Multiple evals with same VM." {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    const src =
        \\use test
        \\a := 1
        \\test.eq(a, 1)
        ;

    var eval_res: C.EvalResult = undefined;
    var res = C.vm_eval(vm, src, &eval_res);
    try zt.eq(C.Success, res);

    res = C.vm_eval(vm, src, &eval_res);
    try zt.eq(C.Success, res);

    res = C.vm_eval(vm, src, &eval_res);
    try zt.eq(C.Success, res);
}

const Endian = enum {
    little,
    big,
};

test "meta constants" {
    const vm = C.vm_init();
    defer C.vm_deinit(vm);

    var eval_res: C.EvalResult = undefined;
    var res = C.vm_eval(vm, 
        \\use meta
        \\fn main() -> SystemKind:
        \\  return meta.system()
    , &eval_res);
    try zt.eq(C.Success, res);
    const exp_system = std.enums.nameCast(meta_mod.SystemKind, builtin.os.tag);
    try zt.eq(@intFromEnum(exp_system), @as(*i64, @ptrCast(eval_res.res)).*);

    res = C.vm_eval(vm, 
        \\use meta
        \\fn main() -> str:
        \\  return meta.cpu()
    , &eval_res);
    try zt.eq(C.Success, res);
    const exp_cpu = @tagName(builtin.cpu.arch);
    try zt.eqStr(exp_cpu, C.str_bytes(@as(*C.str, @ptrCast(eval_res.res)).*));

    res = C.vm_eval(vm, 
        \\use meta
        \\fn main() -> Endian:
        \\  return meta.endian()
    , &eval_res);
    try zt.eq(C.Success, res);
    const exp_endian = std.enums.nameCast(Endian, builtin.cpu.arch.endian());
    try zt.eq(@intFromEnum(exp_endian), @as(*i64, @ptrCast(eval_res.res)).*);
}

// pub fn panic(msg: []const u8, trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
//     _ = trace;
//     _ = ret_addr;
//     // if (is_wasm) {
//     //     std.debug.print("{s}", .{msg});
//     //     @trap();
//     // } else {
//         // std.debug.defaultPanic(msg, @returnAddress());
//     // }
// }

export fn cl_strlen(s: [*:0]const u8) usize {
    return std.mem.len(s);
}