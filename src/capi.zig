/// Using C headers as the single source of truth.
const std = @import("std");
const cy = @import("cyber.zig");
const c = @cImport({
    @cInclude("include/cyber.h");
});

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
pub const Allocator = c.CLAllocator;
pub fn toAllocator(alloc: std.mem.Allocator) Allocator {
    return .{ .ptr = alloc.ptr, .vtable = @ptrCast(alloc.vtable) };
}
pub fn fromAllocator(alloc: Allocator) std.mem.Allocator {
    return .{ .ptr = @ptrCast(alloc.ptr), .vtable = @ptrCast(@alignCast(alloc.vtable)) };
}
pub fn getLog() LogFn {
    return c.clLog;
}
pub fn setLog(func: LogFn) void {
    c.clLog = func;
}
pub const setNewMockHttp = c.clSetNewMockHttp;
pub const getAllocator = c.clGetAllocator;
pub const reportApiError = c.clReportApiError;
pub const defaultResolver = c.clDefaultResolver;
pub const default_module_loader = c.cl_default_module_loader;
pub const mod_core = c.cl_mod_core;
pub const mod_cy = c.cl_mod_cy;
pub const mod_c = c.cl_mod_c;
pub const mod_meta = c.cl_mod_meta;
pub const mod_math = c.cl_mod_math;
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
pub const getVersion = c.clGetVersion;
pub const getFullVersion = c.clGetFullVersion;
pub const getCommit = c.clGetCommit;
pub const getBuild = c.clGetBuild;
pub const defaultEvalConfig = c.clDefaultEvalConfig;
pub const reset = c.clReset;
pub const runReadyTasks = c.clRunReadyTasks;
pub const defaultCompileConfig = c.clDefaultCompileConfig;
pub const listLen = c.clListLen;
pub const listCap = c.clListCap;
pub fn listGet(vm: *VM, list: Value, idx: usize) Value {
    return c.clListGet(@ptrCast(vm), list, idx);
}
pub fn listSet(vm: *VM, list: Value, idx: usize, val: Value) void {
    c.clListSet(@ptrCast(vm), list, idx, val);
}
pub const asFloat = c.clAsFloat;
pub const float = c.clFloat;
pub fn getType(val: Value) *ZType {
    return @ptrCast(c.clGetType(val));
}
pub const newValueDump = c.clNewValueDump;
pub const newFunc = c.clNewFunc;
pub const newType = c.clNewType;
pub const newEmptyMap = c.clNewEmptyMap;
pub fn symbol(vm: *VM, s: []const u8) Value {
    return c.cl_symbol(vm, to_bytes(s));
}
pub const asSymbol = c.clAsSymbol;
pub fn asString(val: Value) []const u8 {
    return from_bytes(c.clAsString(val));
}
pub const Void = c.CL_VOID;
pub const True = c.CL_TRUE;
pub const False = c.CL_FALSE;
pub const bool_ = c.clBool;
pub const asBoxInt = c.clAsBoxInt;
pub const int = c.clInt;
pub const asBool = c.clAsBool;
pub const asBoxBool = c.clAsBoxBool;
pub const toBool = c.clToBool;
pub const declareDynVar = c.clDeclareDynVar;
pub const declareVar = c.clDeclareVar;
pub const ModuleBindFn = c.CLModuleBindFn;
pub const CreateTypeFn = c.CLCreateTypeFn;
pub const CreatedTypeFn = c.CLCreatedTypeFn;
pub const ResolveAliasFn = c.CLResolveAliasFn;
pub const set_resolver = c.cl_set_resolver;
pub const vm_set_loader = c.cl_vm_set_loader;
pub const set_module_config = c.cl_set_module_config;
pub const mod_set_data = c.cl_mod_set_data;
pub const mod_get_data = c.cl_mod_get_data;
pub const createModule = c.clCreateModule;
pub const vm_printer = c.cl_vm_printer;
pub const vm_set_printer = c.cl_vm_set_printer;
pub const vm_eprinter = c.cl_vm_eprinter;
pub const vm_set_eprinter = c.cl_vm_set_eprinter;
pub const vm_set_logger = c.cl_vm_set_logger;
pub const setUserData = c.clSetUserData;
pub const getUserData = c.clGetUserData;

pub fn vm_main_thread(vm: *VM) *Thread {
    return c.cl_vm_main_thread(vm).?;
}

pub const resultName = c.clResultName;
pub const STR = c.CL_STR;
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
pub fn from_bytes(s: Bytes) []const u8 {
    if (s.len == 0) {
        return "";
    }
    return s.ptr[0..s.len];
}
pub fn to_bytes(s: []const u8) Bytes {
    return .{
        .ptr = s.ptr,
        .len = s.len,
    };
}
pub const NullStr = Bytes{ .ptr = null, .len = 0 };
pub const NullSlice = Slice{ .ptr = null, .len = 0};
pub const NullId = std.math.maxInt(u32);

pub const Node = c.CLNode;
pub fn fromNode(node: *Node) *cy.ast.Node {
    return @ptrCast(node);
}
pub fn toNode(node: *cy.ast.Node) *Node {
    return @ptrCast(node);
}

pub const ValueSlice = c.CLValueSlice;
pub const HostFn = c.CLHostFn;
pub const SemaFn = c.CLSemaFn;
pub const BindGlobal = c.CLBindGlobal;
pub const BindFunc = c.CLBindFunc;
pub const PrintFn = c.CLPrintFn;
pub const PrintErrorFn = c.CLPrintErrorFn;
pub const LogFn = c.CLLogFn;
pub const ValidateConfig = c.CLValidateConfig;
pub const CompileConfig = c.CLCompileConfig;
pub const EvalConfig = c.CLEvalConfig;
pub const ModuleLoaderFn = c.CLModuleLoaderFn;
pub const ResolverFn = c.CLResolverFn;
pub const ResolverParams = c.CLResolverParams;
pub const ModuleOnDeinitRtFn = c.CLModuleOnDeinitRtFn;
pub const ModuleOnTypeLoadFn = c.CLModuleOnTypeLoadFn;
pub const ModuleOnLoadFn = c.CLModuleOnLoadFn;
pub const ModuleOnDestroyFn = c.CLModuleOnDestroyFn;
pub const BindType = c.CLBindType;
pub const FuncType = c.CLFuncType;
pub const FuncTypeStandard = c.CL_FUNC_STANDARD;
pub const FuncTypeInline = c.CL_FUNC_INLINE;
pub const Value = c.CLValue;
pub const Ret = c.CLRet;
pub const RetOk: Ret = c.CL_RET_OK;
pub const RetInterrupt: Ret = c.CL_RET_INTERRUPT;
pub const VM = c.CLVM;
pub const Thread = c.CLThread;
pub const Heap = c.CLHeap;

pub fn resolve(vm: *VM, uri: []const u8) []const u8 {
    return from_bytes(c.cl_resolve(vm, to_bytes(uri)));
}

pub const vm_init = c.cl_vm_init;
pub const vm_reset = c.cl_vm_reset;
pub const vm_deinit = c.cl_vm_deinit;

pub fn vm_evalx(vm: *VM, uri: []const u8, src: []const u8, config: EvalConfig, out: *EvalResult) ResultCode {
    return c.cl_vm_evalx(vm, to_bytes(uri), to_bytes(src), config, out);
}

pub fn vm_error_summary(vm: *VM) []const u8 {
    return from_bytes(c.cl_vm_error_summary(vm));
}

pub fn vm_compile_error_summary(vm: *VM) []const u8 {
    return from_bytes(c.cl_vm_compile_error_summary(vm));
}

pub fn thread_panic_summary(t: *Thread) []const u8 {
    return from_bytes(c.cl_thread_panic_summary(t));
}

pub fn vm_free(vm: *VM, bytes: []const u8) void {
    c.cl_vm_free(vm, to_bytes(bytes));
}

pub fn value_desc(vm: *VM, val_t: TypeId, val: ?*Value) []const u8 {
    return from_bytes(c.cl_value_desc(vm, val_t, val));
}

pub const thread_dump_live_objects = c.cl_thread_dump_live_objects;

// pub fn runReadyTasks(self: *ZVM) ResultCode {
//     return c.clRunReadyTasks(@ptrCast(self));
// }

pub const thread_rc = c.cl_thread_rc;
pub const thread_release = c.cl_thread_release;
pub const thread_retain = c.cl_thread_retain;
pub const thread_count_objects = c.cl_thread_count_objects;
pub const str_deinit = c.cl_str_deinit;

pub fn str_init(t: *Thread, s: []const u8) Value {
    return c.cl_str_init(@ptrCast(t), to_bytes(s));
}

// pub const ZVM = struct {
//     pub fn evalMust(self: *ZVM, src: []const u8, out: *Value) void {
//         const res = self.eval(src, out);
//         if (res != Success) {
//             const summary = self.new_error_summary();
//             std.debug.panic("{s}", .{summary});
//         }
//     }

//     pub fn eval(self: *ZVM, src: []const u8, out: *EvalResult) ResultCode {
//         return c.clEval(@ptrCast(self), toStr(src), out);
//     }

//     pub fn compile(self: *ZVM, uri: []const u8, src: []const u8, config: CompileConfig) ResultCode {
//         return c.clCompile(@ptrCast(self), toStr(uri), toStr(src), config);
//     }

//     pub fn validate(self: *ZVM, src: []const u8) ResultCode {
//         return c.clValidate(@ptrCast(self), toStr(src));
//     }

//     pub fn collectCycles(self: *ZVM) GCResult {
//         return c.clCollectCycles(@ptrCast(self));
//     }

//     pub fn newInt(self: *ZVM, n: i64) Value {
//         return c.clNewInt(@ptrCast(self), n);
//     }

//     pub fn newPointerVoid(self: *ZVM, ptr: *anyopaque) Value {
//         return c.clNewPointerVoid(@ptrCast(self), ptr);
//     }

//     pub fn newEmptyMap(self: *ZVM) Value {
//         return c.clNewEmptyMap(@ptrCast(self));
//     }

//     pub fn newFuncUnion(self: *ZVM, union_t: *cy.Type, ptr: HostFn) Value {
//         return c.clNewFuncUnion(@ptrCast(self), @ptrCast(union_t), ptr);
//     }

//     pub fn newInstance(self: *ZVM, type_: *ZType, fields: []const FieldInit) Value {
//         return c.clNewInstance(@ptrCast(self), @ptrCast(type_), fields.ptr, fields.len);
//     }

//     pub fn newNone(self: *ZVM, option_t: *ZType) Value {
//         return @bitCast(c.clNewNone(@ptrCast(self), @ptrCast(option_t)));
//     }

//     pub fn newSome(self: *ZVM, option_t: *ZType, val: Value) !Value {
//         return @bitCast(c.clNewSome(@ptrCast(self), @ptrCast(option_t), @bitCast(val)));
//     }

//     pub fn newChoice(self: *ZVM, choice_t: *ZType, name: []const u8, val: Value) Value {
//         return c.clNewChoice(@ptrCast(self), @ptrCast(choice_t), toStr(name), val);
//     }

//     pub fn findType(self: *ZVM, spec: []const u8) ?*ZType {
//         return @ptrCast(@alignCast(c.clFindType(@ptrCast(self), toStr(spec))));
//     }

//     pub fn getField(self: *ZVM, rec: Value, name: []const u8) Value {
//         return c.clGetField(@ptrCast(self), rec, toStr(name));
//     }

//     pub fn unwrapChoice(self: *ZVM, choice_t: *ZType, choice: Value, name: []const u8) Value {
//         return c.clUnwrapChoice(@ptrCast(self), @ptrCast(choice_t), choice, toStr(name));
//     }

//     pub fn expandTemplateType(self: *ZVM, template: Sym, args: []const Value) ?*ZType {
//         return @ptrCast(@alignCast(c.clExpandTemplateType(@ptrCast(self), template, args.ptr, args.len)));
//     }
// };

pub const Unit = c.CLUnit;
pub const Sym = c.CLSym;
pub const GCResult = c.CLGCResult;
pub const FieldInit = c.CLFieldInit;
pub fn toFieldInit(name: []const u8, val: Value) FieldInit {
    return .{ .name = to_bytes(name), .value = val };
}

pub const FuncEnumType = enum(u8) {
    standard = c.CL_FUNC_STANDARD,
    inlinec = c.CL_FUNC_INLINE,
};

pub const BindTypeDecl = c.CL_BIND_TYPE_DECL;
pub const BindTypeCreate = c.CL_BIND_TYPE_CREATE;
pub const BindTypeAlias = c.CL_BIND_TYPE_ALIAS;

pub const BindFuncVm = c.CL_BIND_FUNC_VM;
pub const BindFuncCt = c.CL_BIND_FUNC_CT;
pub const BindFuncSema = c.CL_BIND_FUNC_SEMA;
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

pub const str = c.CL_str;

pub const Type = c.CLType;
pub const TypeId = c.CLTypeId;
pub const TypeNull = c.CL_TYPE_NULL;
pub const TypeVoid = c.CL_TYPE_VOID;
pub const TypeBoolean = c.CL_TYPE_BOOLEAN;
pub const TypeError = c.CL_TYPE_ERROR;
pub const TypeSymbol = c.CL_TYPE_SYMBOL;
pub const TypeInt = c.CL_TYPE_INT;
pub const TypeFloat = c.CL_TYPE_FLOAT;
pub const TypeFunc = c.CL_TYPE_FUNC;
pub const TypeStr = c.CL_TYPE_STR;
pub const TypeFiber = c.CL_TYPE_FIBER;
pub const TypeExternFunc = c.CL_TYPE_EXTERN_FUNC;
pub const TypeTccState = c.CL_TYPE_TCCSTATE;
pub const TypeTuple = c.CL_TYPE_TUPLE;
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

pub fn field_init(name: []const u8, val: cy.Value) FieldInit {
    return .{ .name = to_bytes(name), .value = @bitCast(val) };
}