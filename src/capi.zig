/// Zig API to `libcyber.h`.
const std = @import("std");
const builtin = @import("builtin");
pub const c = @cImport({
    @cInclude("include/cyber.h");
});

pub fn TRACE() bool {
    return c.CL_TRACE;
}

pub fn verbose() bool {
    return c.clVerbose;
}
pub fn setVerbose(v: bool) void {
    c.clVerbose = v;
}
pub fn silent() bool {
    return c.clSilent;
}
pub fn setSilent(v: bool) void {
    c.clSilent = v;
}

pub fn set_logger(logger_: GlobalLogFn) void {
    c.cl_logger = logger_;
}

pub fn logger() GlobalLogFn {
    return c.cl_logger;
}

pub const Allocator = c.CLAllocator;
pub fn toAllocator(alloc: std.mem.Allocator) Allocator {
    return .{ .ptr = alloc.ptr, .vtable = @ptrCast(alloc.vtable) };
}
pub fn fromAllocator(alloc: Allocator) std.mem.Allocator {
    @setRuntimeSafety(false);
    return .{ .ptr = @ptrCast(alloc.ptr), .vtable = @ptrCast(@alignCast(alloc.vtable)) };
}
// pub const setNewMockHttp = c.clSetNewMockHttp;
pub const reportApiError = c.clReportApiError;
pub const default_resolver = c.cl_default_resolver;
pub const default_loader = c.cl_default_loader;
pub const mod_bind_core = c.cl_mod_bind_core;
pub const mod_bind_cy = c.cl_mod_bind_cy;
pub const mod_bind_c = c.cl_mod_bind_c;
pub const mod_bind_meta = c.cl_mod_bind_meta;
pub const mod_bind_math = c.cl_mod_bind_math;
pub const mod_bind_io = c.cl_mod_bind_io;
pub const mod_bind_test = c.cl_mod_bind_test;
pub const mod_set_fallback_func = c.cl_mod_set_fallback_func;
pub fn mod_add_func(mod: *Sym, name: []const u8, binding: BindFunc) void {
    c.cl_mod_add_func(mod, to_bytes(name), binding);
}
pub fn mod_add_type(mod: *Sym, name: []const u8, binding: BindType) void {
    c.cl_mod_add_type(mod, to_bytes(name), binding);
}
pub fn mod_add_global(mod: *Sym, name: []const u8, binding: BindGlobal) void {
    c.cl_mod_add_global(mod, to_bytes(name), binding);
}
pub const mod_on_destroy = c.cl_mod_on_destroy;
pub const mod_on_load = c.cl_mod_on_load;
pub fn version() []const u8 {
    return from_bytes(c.cl_version());
}
pub fn full_version() []const u8 {
    return from_bytes(c.cl_full_version());
}
pub fn commit() []const u8 {
    return from_bytes(c.cl_commit());
}
pub fn build() []const u8 {
    return from_bytes(c.cl_build());
}
pub const defaultEvalConfig = c.clDefaultEvalConfig;
// pub const reset = c.clReset;
// pub const runReadyTasks = c.clRunReadyTasks;
pub const defaultCompileConfig = c.clDefaultCompileConfig;
pub fn getType(val: Value) *ZType {
    return @ptrCast(c.clGetType(val));
}
// pub const newFunc = c.clNewFunc;
// pub const newType = c.clNewType;
// pub const newEmptyMap = c.clNewEmptyMap;
pub fn symbol(vm: *VM, s: []const u8) i64 {
    return c.cl_symbol(vm, to_bytes(s));
}
pub const ModuleBindFn = c.CLModuleBindFn;
pub const CreateTypeFn = c.CLCreateTypeFn;
pub const ResolveAliasFn = c.CLResolveAliasFn;
pub fn vm_allocator(vm: *VM) std.mem.Allocator {
    return fromAllocator(c.cl_vm_allocator(vm));
}
pub const vm_set_resolver = c.cl_vm_set_resolver;
pub const vm_set_dl_resolver = c.cl_vm_set_dl_resolver;
pub const vm_set_loader = c.cl_vm_set_loader;
pub const mod_set_data = c.cl_mod_set_data;
pub const mod_get_data = c.cl_mod_get_data;
pub const vm_printer = c.cl_vm_printer;
pub const vm_set_printer = c.cl_vm_set_printer;
pub const vm_eprinter = c.cl_vm_eprinter;
pub const vm_set_eprinter = c.cl_vm_set_eprinter;
pub const vm_set_logger = c.cl_vm_set_logger;
pub const vm_logger = c.cl_vm_logger;
pub const vm_set_user_data = c.cl_vm_set_user_data;
pub const vm_user_data = c.cl_vm_user_data;
pub const vm_dump_bytecode = c.cl_vm_dump_bytecode;
pub const TraceInfo = c.CLTraceInfo;

pub fn vm_main_thread(vm: *VM) *Thread {
    return c.cl_vm_main_thread(vm).?;
}

pub const resultName = c.clResultName;
pub const BYTES = c.CL_BYTES;
pub inline fn TYPE_RESERVE_DECL(type_id: TypeId) BindType {
    return BindType{
        .type = c.CL_BIND_TYPE_DECL,
        .data = .{ .decl = .{
            .type_id = type_id,
        }},
    };
}
pub inline fn TYPE_DECL() BindType {
    return BindType{
        .type = c.CL_BIND_TYPE_DECL,
        .data = .{ .decl = .{
            .type_id = NullId,
        }},
    };
}
pub inline fn TYPE_CREATE(create_fn: CreateTypeFn) BindType {
    return BindType{
        .type = c.CL_BIND_TYPE_CREATE,
        .data = .{ .create = .{
            .create_fn = create_fn,
        }},
    };
}
pub inline fn TYPE_ALIAS(resolve_fn: ResolveAliasFn) BindType {
    return BindType{
        .type = c.CL_BIND_TYPE_ALIAS,
        .data = .{ .alias = .{
            .resolve_fn = resolve_fn,
        }},
    };
}

pub const Slice = c.CLSlice;
pub fn fromSlice(comptime T: type, s: Slice) []const T {
    if (s.len == 0) {
        return &.{};
    }
    return @as([*]T, @ptrCast(@alignCast(s.ptr)))[0..s.len];
}
pub fn toSlice(comptime T: type, s: []const T) Slice {
    return .{
        .ptr = @constCast(s.ptr),
        .len = s.len,
    };
}
pub const Bytes = c.CLBytes;
pub fn from_bytes(s: Bytes) []u8 {
    if (s.len == 0) {
        return "";
    }
    return s.ptr[0..s.len];
}
pub fn to_bytes(s: []const u8) Bytes {
    return .{
        .ptr = @constCast(s.ptr),
        .len = s.len,
    };
}
pub const NullStr = Bytes{ .ptr = null, .len = 0 };
pub const NullSlice = Slice{ .ptr = null, .len = 0};
pub const NullId = std.math.maxInt(u32);

pub const Node = c.CLNode;

pub const ValueSlice = c.CLValueSlice;
pub const HostFn = c.CLHostFn;
pub const SemaFn = c.CLSemaFn;
pub const BindGlobal = c.CLBindGlobal;
pub const BindFunc = c.CLBindFunc;
pub const PrintFn = c.CLPrintFn;
pub const PrintErrorFn = c.CLPrintErrorFn;
pub const LogFn = c.CLLogFn;
pub const GlobalLogFn = c.CLGlobalLogFn;
pub const ValidateConfig = c.CLValidateConfig;
pub const CompileConfig = c.CLCompileConfig;
pub const EvalConfig = c.CLEvalConfig;
pub const ModuleLoaderFn = c.CLModuleLoaderFn;
pub const LoaderResult = c.CLLoaderResult;
pub const ResolverFn = c.CLResolverFn;
pub const ResolverParams = c.CLResolverParams;
pub const DlResolverFn = c.CLDlResolverFn;
pub const ModuleOnLoadFn = c.CLModuleOnLoadFn;
pub const ModuleOnDestroyFn = c.CLModuleOnDestroyFn;
pub const BindType = c.CLBindType;
// pub const FuncType = c.CLFuncType;
// pub const FuncTypeStandard = c.CL_FUNC_STANDARD;
// pub const FuncTypeInline = c.CL_FUNC_INLINE;
pub const Value = c.CLValue;
pub const Ret = c.CLRet;
pub const RetOk: Ret = c.CL_RET_OK;
pub const RetInterrupt: Ret = c.CL_RET_INTERRUPT;
pub const VM = c.CLVM;
pub const Thread = c.CLThread;
pub const Heap = c.CLHeap;
pub const StackFrame = c.CLStackFrame;
pub const StackTrace = c.CLStackTrace;

pub fn vm_resolve(vm: *VM, uri: []const u8) []const u8 {
    return from_bytes(c.cl_vm_resolve(vm, to_bytes(uri)));
}

pub fn vm_init() *VM {
    return c.cl_vm_init().?;
}
pub fn vm_initx(allocator: std.mem.Allocator) *VM {
    return c.cl_vm_initx(toAllocator(allocator)).?;
}
pub const vm_reset = c.cl_vm_reset;
pub const vm_deinit = c.cl_vm_deinit;

pub fn vm_compile(vm: *VM, uri: []const u8, src: []const u8, config: CompileConfig) ResultCode {
    return c.cl_vm_compile(vm, to_bytes(uri), to_bytes(src), config);
}

pub fn vm_compile_path(vm: *VM, uri: []const u8, config: CompileConfig) ResultCode {
    return c.cl_vm_compile_path(vm, to_bytes(uri), config);
}

pub fn vm_validate(vm: *VM, src: []const u8) ResultCode {
    return c.cl_vm_validate(vm, to_bytes(src));
}

pub fn vm_eval(vm: *VM, src: []const u8, out: *EvalResult) ResultCode {
    return c.cl_vm_eval(vm, to_bytes(src), out);
}

pub fn vm_evalx(vm: *VM, uri: []const u8, src: []const u8, config: EvalConfig, out: *EvalResult) ResultCode {
    return c.cl_vm_evalx(vm, to_bytes(uri), to_bytes(src), config, out);
}

pub fn vm_eval_path(vm: *VM, uri: []const u8, config: EvalConfig, out: *EvalResult) ResultCode {
    return c.cl_vm_eval_path(vm, to_bytes(uri), config, out);
}

pub fn vm_error_summary(vm: *VM) []const u8 {
    return from_bytes(c.cl_vm_error_summary(vm));
}

pub fn vm_compile_error_summary(vm: *VM) []const u8 {
    return from_bytes(c.cl_vm_compile_error_summary(vm));
}

pub const thread_panic_trace = c.cl_thread_panic_trace;

pub fn thread_panic_summary(t: *Thread) []const u8 {
    return from_bytes(c.cl_thread_panic_summary(t));
}

pub fn vm_allocb(vm: *VM, size: usize) []u8 {
    return from_bytes(c.cl_vm_allocb(vm, size));
}

pub fn vm_freeb(vm: *VM, bytes: []const u8) void {
    c.cl_vm_freeb(vm, to_bytes(bytes));
}

pub fn value_desc(vm: *VM, val_t: TypeId, val: ?*Value) []const u8 {
    return from_bytes(c.cl_value_desc(vm, val_t, val));
}

pub fn thread_vm(t: *Thread) *VM {
    return c.cl_thread_vm(t).?;
}

pub fn thread_allocator(t: *Thread) std.mem.Allocator {
    return fromAllocator(c.cl_thread_allocator(t));
}
pub const thread_dump_live_objects = c.cl_thread_dump_live_objects;
pub const thread_trace_info = c.cl_thread_trace_info;
pub const thread_signal_host_panic = c.cl_thread_signal_host_panic;
pub const thread_signal_host_segfault = c.cl_thread_signal_host_segfault;

// pub fn runReadyTasks(self: *ZVM) ResultCode {
//     return c.clRunReadyTasks(@ptrCast(self));
// }

pub const thread_dump_stats = c.cl_thread_dump_stats;
pub const thread_rc = c.cl_thread_rc;
pub const thread_release = c.cl_thread_release;
pub const thread_retain = c.cl_thread_retain;
pub const thread_count_objects = c.cl_thread_count_objects;
pub const str_deinit = c.cl_str_deinit;

pub fn str_bytes(s: str) []const u8 {
    return from_bytes(c.cl_str_bytes(s));
}

pub fn thread_ret(t: *Thread, comptime R: type) *R {
    return @ptrCast(@alignCast(c.cl_thread_ret(t, @sizeOf(R))));
}

pub fn thread_ret_panic(t: *Thread, msg: []const u8) Ret {
    return c.cl_thread_ret_panic(t, to_bytes(msg));
}

pub const thread_int = c.cl_thread_int;
pub const thread_float = c.cl_thread_float;
pub const thread_str = c.cl_thread_str;
pub const thread_slice = c.cl_thread_slice;
pub const slice_init = c.cl_slice_init;

pub fn slice_items(slice: Slice, comptime E: type) []E {
    var buf: [*]E = @ptrCast(@alignCast(slice.ptr));
    return buf[0..@intCast(slice.len)];
}

pub fn thread_param(t: *Thread, comptime P: type) P {
    const ptr: *P = @ptrCast(@alignCast(c.cl_thread_param(t, @sizeOf(P))));
    return ptr.*;
}

pub fn str_init(t: *Thread, s: []const u8) str {
    return c.cl_str_init(@ptrCast(t), to_bytes(s));
}

// pub const ZVM = struct {
//     pub fn collectCycles(self: *ZVM) GCResult {
//         return c.clCollectCycles(@ptrCast(self));
//     }

//     pub fn newEmptyMap(self: *ZVM) Value {
//         return c.clNewEmptyMap(@ptrCast(self));
//     }

//     pub fn newFuncUnion(self: *ZVM, union_t: *cy.Type, ptr: HostFn) Value {
//         return c.clNewFuncUnion(@ptrCast(self), @ptrCast(union_t), ptr);
//     }

//     pub fn expandTemplateType(self: *ZVM, template: Sym, args: []const Value) ?*ZType {
//         return @ptrCast(@alignCast(c.clExpandTemplateType(@ptrCast(self), template, args.ptr, args.len)));
//     }
// };

pub const Unit = c.CLUnit;
pub const Sym = c.CLSym;
pub const MemoryCheck = c.CLMemoryCheck;
pub const FieldInit = c.CLFieldInit;
pub fn toFieldInit(name: []const u8, val: Value) FieldInit {
    return .{ .name = to_bytes(name), .value = val };
}

// pub const FuncEnumType = enum(u8) {
//     standard = c.CL_FUNC_STANDARD,
//     inlinec = c.CL_FUNC_INLINE,
// };

pub const BindTypeDecl = c.CL_BIND_TYPE_DECL;
pub const BindTypeCreate = c.CL_BIND_TYPE_CREATE;
pub const BindTypeAlias = c.CL_BIND_TYPE_ALIAS;

pub const BIND_FUNC = c.CL_BIND_FUNC;
pub const BIND_GLOBAL = c.CL_BIND_GLOBAL;

pub const BindFuncVm = c.CL_BIND_FUNC_VM;
pub const BindFuncConstEval = c.CL_BIND_FUNC_CONST_EVAL;
pub const BindFuncBuiltin = c.CL_BIND_FUNC_BUILTIN;
pub const BindFuncDecl = c.CL_BIND_FUNC_DECL;

pub const Backend = c.CLBackend;
pub const BackendVM = c.CL_VM;
pub const BackendJIT = c.CL_JIT;
pub const BackendTCC = c.CL_TCC;
pub const BackendCC = c.CL_CC;
pub const BackendLLVM = c.CL_LLVM;

pub const ZType = struct {
    pub fn id(self: *ZType) TypeId {
        return c.clTypeId(@ptrCast(self));
    }
};

pub const str = c.CLstr;

pub const Type = c.CLType;
pub const TypeId = c.CLTypeId;
pub const TypeNull = c.CL_TYPE_NULL;
pub const TypeVoid = c.CL_TYPE_VOID;
pub const TypeBool = c.CL_TYPE_BOOL;
pub const TypeError = c.CL_TYPE_ERROR;
pub const TypeSymbol = c.CL_TYPE_SYMBOL;
pub const TypeI64 = c.CL_TYPE_I64;
pub const TypeF64 = c.CL_TYPE_F64;
pub const TypeStr = c.CL_TYPE_STR;
pub const TypeType = c.CL_TYPE_TYPE;
pub const TypeFuncSig = c.CL_TYPE_FUNC_SIG;
pub const TypeCode = c.CL_TYPE_CODE;

pub const EvalResult = c.CLEvalResult;
pub const ResultCode = c.CLResultCode;
pub const Success = c.CL_SUCCESS;
pub const Await = c.CL_AWAIT;
pub const ErrorCompile = c.CL_ERROR_COMPILE;
pub const ErrorPanic = c.CL_ERROR_PANIC;
pub const ErrorUnknown = c.CL_ERROR_UNKNOWN;

pub fn vm_find_type(vm: *VM, spec: []const u8) ?*Type {
    return c.cl_find_type(vm, to_bytes(spec));
}