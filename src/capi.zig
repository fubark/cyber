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
pub const defaultModuleLoader = c.clDefaultModuleLoader;
pub const getVersion = c.clGetVersion;
pub const getFullVersion = c.clGetFullVersion;
pub const getCommit = c.clGetCommit;
pub const getBuild = c.clGetBuild;
pub fn create() *ZVM {
    return @ptrCast(c.clCreate());
}
pub const defaultEvalConfig = c.clDefaultEvalConfig;
pub const reset = c.clReset;
pub const eval = c.clEval;
pub const evalExt = c.clEvalExt;
pub const runReadyTasks = c.clRunReadyTasks;
pub const defaultCompileConfig = c.clDefaultCompileConfig;
pub const listLen = c.clListLen;
pub const listCap = c.clListCap;
pub fn listAppend(vm: *ZVM, list: Value, val: Value) void {
    c.clListAppend(@ptrCast(vm), list, val);
}
pub fn listInsert(vm: *ZVM, list: Value, idx: usize, val: Value) void {
    c.clListInsert(@ptrCast(vm), list, idx, val);
}
pub fn listGet(vm: *ZVM, list: Value, idx: usize) Value {
    return c.clListGet(@ptrCast(vm), list, idx);
}
pub fn listSet(vm: *ZVM, list: Value, idx: usize, val: Value) void {
    c.clListSet(@ptrCast(vm), list, idx, val);
}
pub const asFloat = c.clAsFloat;
pub const float = c.clFloat;
pub const getType = c.clGetType;
pub const isFuture = c.clIsFuture;
pub const newValueDump = c.clNewValueDump;
pub const newFunc = c.clNewFunc;
pub const newType = c.clNewType;
pub const newEmptyMap = c.clNewEmptyMap;
pub fn symbol(vm: *ZVM, str: []const u8) Value {
    return c.clSymbol(@ptrCast(vm), toStr(str));
}
pub const asSymbolId = c.clAsSymbolId;
pub fn asString(val: Value) []const u8 {
    return fromStr(c.clAsString(val));
}
pub const clTrue = c.clTrue;
pub const clFalse = c.clFalse;
pub const asBoxInt = c.clAsBoxInt;
pub const int = c.clInt;
pub const asBool = c.clAsBool;
pub const toBool = c.clToBool;
pub const declareFuncDyn = c.clDeclareFuncDyn;
pub const declareFunc = c.clDeclareFunc;
pub const declareDynVar = c.clDeclareDynVar;
pub const declareVar = c.clDeclareVar;
pub const CreateTypeFn = c.CLCreateTypeFn;
pub const setResolver = c.clSetResolver;
pub const setModuleLoader = c.clSetModuleLoader;
pub const setModuleConfig = c.clSetModuleConfig;
pub const createModule = c.clCreateModule;
pub const getPrinter = c.clGetPrinter;
pub const setPrinter = c.clSetPrinter;
pub const getErrorPrinter = c.clGetErrorPrinter;
pub const setErrorPrinter = c.clSetErrorPrinter;
pub const setUserData = c.clSetUserData;
pub const getUserData = c.clGetUserData;
pub const resultName = c.clResultName;
pub const STR = c.CL_STR;
pub inline fn CORE_TYPE(type_id: Type) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_CORE_CUSTOM,
        .data = .{ .core_custom = .{
            .type_id = type_id,
            .get_children = null,
            .finalizer = null,
        }},
    };
}
pub inline fn CORE_TYPE_EXT(type_id: Type, get_children: GetChildrenFn, finalizer: FinalizerFn, load_all_methods: bool) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_CORE_CUSTOM,
        .data = .{ .core_custom = .{
            .type_id = type_id,
            .get_children = get_children,
            .finalizer = finalizer,
            .load_all_methods = load_all_methods,
        }},
    };
}
pub inline fn DECL_TYPE(type_id: Type) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_DECL,
        .data = .{ .decl = .{
            .type_id = type_id,
            .out_type_id = null,
        }},
    };
}
pub inline fn DECL_TYPE_GET(out_type_id: *Type) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_DECL,
        .data = .{ .decl = .{
            .type_id = NullId,
            .out_type_id = out_type_id,
        }},
    };
}
pub inline fn CREATE_TYPE(create_fn: CreateTypeFn) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_CREATE,
        .data = .{ .create = .{
            .create_fn = create_fn,
        }},
    };
}
pub inline fn HOST_OBJECT(out_type_id: ?*Type, get_children: GetChildrenFn, finalizer: FinalizerFn) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_HOSTOBJ,
        .data = .{ .hostobj = .{
            .out_type_id = out_type_id,
            .get_children = get_children,
            .finalizer = finalizer,
            .pre = false,
        }},
    };
}
pub inline fn HOST_OBJECT_PRE(out_type_id: ?*Type, get_children: GetChildrenFn, finalizer: FinalizerFn) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_HOSTOBJ,
        .data = .{ .hostobj = .{
            .out_type_id = out_type_id,
            .get_children = get_children,
            .finalizer = finalizer,
            .pre = true,
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
pub const Str = c.CLStr;
pub fn fromStr(str: Str) []const u8 {
    if (str.len == 0) {
        return "";
    }
    return str.ptr[0..str.len];
}
pub fn toStr(s: []const u8) Str {
    return .{
        .ptr = s.ptr,
        .len = s.len,
    };
}
pub const NullStr = Str{ .ptr = null, .len = 0 };
pub const NullSlice = Slice{ .ptr = null, .len = 0};
pub const NullId = std.math.maxInt(u32);

pub const Node = c.CLNode;
pub fn fromNode(node: Node) *cy.ast.Node {
    return @ptrCast(@alignCast(node.ptr));
}
pub fn toNode(node: *cy.ast.Node) Node {
    return Node{ .ptr = node };
}

pub const ValueSlice = c.CLValueSlice;
pub const FuncFn = c.CLFuncFn;
pub const PrintFn = c.CLPrintFn;
pub const PrintErrorFn = c.CLPrintErrorFn;
pub const LogFn = c.CLLogFn;
pub const ValidateConfig = c.CLValidateConfig;
pub const CompileConfig = c.CLCompileConfig;
pub const EvalConfig = c.CLEvalConfig;
pub const FuncInfo = c.CLFuncInfo;
pub const ModuleLoaderFn = c.CLModuleLoaderFn;
pub const ResolverFn = c.CLResolverFn;
pub const ResolverParams = c.CLResolverParams;
pub const FuncLoaderFn = c.CLFuncLoaderFn;
pub const VarLoaderFn = c.CLVarLoaderFn;
pub const TypeLoaderFn = c.CLTypeLoaderFn;
pub const ModuleOnDeinitRtFn = c.CLModuleOnDeinitRtFn;
pub const Module = c.CLModule;
pub const ModuleConfig = c.CLModuleConfig;
pub const ModuleOnTypeLoadFn = c.CLModuleOnTypeLoadFn;
pub const ModuleOnLoadFn = c.CLModuleOnLoadFn;
pub const ModuleOnDestroyFn = c.CLModuleOnDestroyFn;
pub const TypeInfo = c.CLTypeInfo;
pub const HostType = c.CLHostType;
pub const HostTypeEntry = c.CLHostTypeEntry;
pub fn hostTypeEntry(name: []const u8, host_t: HostType) HostTypeEntry {
    return .{
        .name = toStr(name),
        .host_t = host_t,
    };
}
pub const HostFuncEntry = c.CLHostFuncEntry;
pub const FuncType = c.CLFuncType;
pub const FuncTypeStandard = c.CL_FUNC_STANDARD;
pub const FuncTypeInline = c.CL_FUNC_INLINE;
pub const VarInfo = c.CLVarInfo;
pub const VarResult = c.CLVarResult;
pub const Value = c.CLValue;
pub const VM = c.CLVM;
pub const ZVM = struct {

    pub fn deinit(self: *ZVM) void {
        c.clDeinit(@ptrCast(self));
    }

    pub fn destroy(self: *ZVM) void {
        c.clDestroy(@ptrCast(self));
    }

    pub fn reset(self: *ZVM) void {
        c.clReset(@ptrCast(self));
    }

    pub fn eval(self: *ZVM, src: []const u8, out: *Value) ResultCode {
        return c.clEval(@ptrCast(self), toStr(src), out);
    }

    pub fn evalExt(self: *ZVM, uri: []const u8, src: []const u8, config: EvalConfig, out: *Value) ResultCode {
        return c.clEvalExt(@ptrCast(self), toStr(uri), toStr(src), config, out);
    }

    pub fn compile(self: *ZVM, uri: []const u8, src: []const u8, config: CompileConfig) ResultCode {
        return c.clCompile(@ptrCast(self), toStr(uri), toStr(src), config);
    }

    pub fn validate(self: *ZVM, src: []const u8) ResultCode {
        return c.clValidate(@ptrCast(self), toStr(src));
    }

    pub fn runReadyTasks(self: *ZVM) ResultCode {
        return c.clRunReadyTasks(@ptrCast(self));
    }

    pub fn traceDumpLiveObjects(self: *ZVM) void {
        c.clTraceDumpLiveObjects(@ptrCast(self));
    }

    pub fn performGC(self: *ZVM) GCResult {
        return c.clPerformGC(@ptrCast(self));
    }

    pub fn countObjects(self: *ZVM) usize {
        return c.clCountObjects(@ptrCast(self));
    }

    pub fn getGlobalRC(self: *ZVM) usize {
        return c.clGetGlobalRC(@ptrCast(self));
    }

    pub fn newLastErrorSummary(self: *ZVM) []const u8 {
        return fromStr(c.clNewLastErrorSummary(@ptrCast(self)));
    }

    pub fn newErrorReportSummary(self: *ZVM) []const u8 {
        return fromStr(c.clNewErrorReportSummary(@ptrCast(self)));
    }

    pub fn newPanicSummary(self: *ZVM) []const u8 {
        return fromStr(c.clNewPanicSummary(@ptrCast(self)));
    }

    pub fn resolve(self: *ZVM, uri: []const u8) []const u8 {
        return fromStr(c.clResolve(@ptrCast(self), toStr(uri)));
    }

    pub fn release(self: *ZVM, val: Value) void {
        c.clRelease(@ptrCast(self), val);
    }

    pub fn retain(self: *ZVM, val: Value) void {
        c.clRetain(@ptrCast(self), val);
    }

    pub fn newInt(self: *ZVM, n: i64) Value {
        return c.clNewInt(@ptrCast(self), n);
    }

    pub fn newString(self: *ZVM, str: []const u8) Value {
        return c.clNewString(@ptrCast(self), toStr(str));
    }

    pub fn newPointerVoid(self: *ZVM, ptr: *anyopaque) Value {
        return c.clNewPointerVoid(@ptrCast(self), ptr);
    }

    pub fn newEmptyMap(self: *ZVM) Value {
        return c.clNewEmptyMap(@ptrCast(self));
    }

    pub fn newFunc(self: *ZVM, params: []const Type, ret_t: Type, ptr: FuncFn) Value {
        return c.clNewFunc(@ptrCast(self), params.ptr, params.len, ret_t, ptr);
    }

    pub fn newType(self: *ZVM, type_id: Type) Value {
        return c.clNewType(@ptrCast(self), type_id);
    }

    pub fn newValueDump(self: *ZVM, val: Value) []const u8 {
        return fromStr(c.clNewValueDump(@ptrCast(self), val));
    }

    pub fn findType(self: *ZVM, path: []const u8) Type {
        return c.clFindType(@ptrCast(self), toStr(path));
    }

    pub fn getField(self: *ZVM, rec: Value, name: []const u8) Value {
        return c.clGetField(@ptrCast(self), rec, toStr(name));
    }

    pub fn expandTemplateType(self: *ZVM, template: Sym, args: []const Value, res: *Type) bool {
        return c.clExpandTemplateType(@ptrCast(self), template, args.ptr, args.len, res);
    }

    pub fn free(self: *ZVM, bytes: []const u8) void {
        c.clFree(@ptrCast(self), toStr(bytes));
    }
};

pub const Sym = c.CLSym;
pub const GCResult = c.CLGCResult;

pub const GetChildrenFn = c.CLGetChildrenFn;
pub const FinalizerFn = c.CLFinalizerFn;

pub const FuncEnumType = enum(u8) {
    standard = c.CL_FUNC_STANDARD,
    inlinec = c.CL_FUNC_INLINE,
};

pub const BindTypeHostObj = c.CL_BIND_TYPE_HOSTOBJ;
pub const BindTypeCoreCustom = c.CL_BIND_TYPE_CORE_CUSTOM;
pub const BindTypeDecl = c.CL_BIND_TYPE_DECL;
pub const BindTypeCreate = c.CL_BIND_TYPE_CREATE;

pub const Backend = c.CLBackend;
pub const BackendVM = c.CL_VM;
pub const BackendJIT = c.CL_JIT;
pub const BackendTCC = c.CL_TCC;
pub const BackendCC = c.CL_CC;
pub const BackendLLVM = c.CL_LLVM;

pub const Type = c.CLType;
pub const TypeNull = c.CL_TYPE_NULL;
pub const TypeVoid = c.CL_TYPE_VOID;
pub const TypeBoolean = c.CL_TYPE_BOOLEAN;
pub const TypeError = c.CL_TYPE_ERROR;
pub const TypeSymbol = c.CL_TYPE_SYMBOL;
pub const TypeInteger = c.CL_TYPE_INTEGER;
pub const TypeFloat = c.CL_TYPE_FLOAT;
pub const TypeListDyn = c.CL_TYPE_LIST_DYN;
pub const TypeListIterDyn = c.CL_TYPE_LISTITER_DYN;
pub const TypeMap = c.CL_TYPE_MAP;
pub const TypeMapIter = c.CL_TYPE_MAPITER;
pub const TypeFunc = c.CL_TYPE_FUNC;
pub const TypeString = c.CL_TYPE_STRING;
pub const TypeFiber = c.CL_TYPE_FIBER;
pub const TypeExternFunc = c.CL_TYPE_EXTERN_FUNC;
pub const TypeTccState = c.CL_TYPE_TCCSTATE;
pub const TypeTuple = c.CL_TYPE_TUPLE;
pub const TypeType = c.CL_TYPE_TYPE;
pub const TypeFuncSig = c.CL_TYPE_FUNC_SIG;
pub const TypeExprType = c.CL_TYPE_EXPRTYPE;

pub const ResultCode = c.CLResultCode;
pub const Success = c.CL_SUCCESS;
pub const Await = c.CL_AWAIT;
pub const ErrorCompile = c.CL_ERROR_COMPILE;
pub const ErrorPanic = c.CL_ERROR_PANIC;
pub const ErrorUnknown = c.CL_ERROR_UNKNOWN;