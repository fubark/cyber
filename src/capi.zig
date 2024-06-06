/// Using C headers as the single source of truth.
const std = @import("std");
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
pub const create = c.clCreate;
pub const deinit = c.clDeinit;
pub const destroy = c.clDestroy;
pub const validate = c.clValidate;
pub const defaultEvalConfig = c.clDefaultEvalConfig;
pub const reset = c.clReset;
pub const eval = c.clEval;
pub const evalExt = c.clEvalExt;
pub const runReadyTasks = c.clRunReadyTasks;
pub const defaultCompileConfig = c.clDefaultCompileConfig;
pub const compile = c.clCompile;
pub const free = c.clFree;
pub const listLen = c.clListLen;
pub const listCap = c.clListCap;
pub const listAppend = c.clListAppend;
pub const listGet = c.clListGet;
pub const listSet = c.clListSet;
pub const listInsert = c.clListInsert;
pub const asFloat = c.clAsFloat;
pub const float = c.clFloat;
pub const getTypeId = c.clGetTypeId;
pub const isFuture = c.clIsFuture;
pub const newValueDump = c.clNewValueDump;
pub const newFunc = c.clNewFunc;
pub const newPointer = c.clNewPointer;
pub const newType = c.clNewType;
pub const newEmptyMap = c.clNewEmptyMap;
pub const symbol = c.clSymbol;
pub const asSymbolId = c.clAsSymbolId;
pub const clTrue = c.clTrue;
pub const clFalse = c.clFalse;
pub const asInteger = c.clAsInteger;
pub const integer = c.clInteger;
pub const asBool = c.clAsBool;
pub const toBool = c.clToBool;
pub const declareFuncDyn = c.clDeclareFuncDyn;
pub const declareFunc = c.clDeclareFunc;
pub const declareDynVar = c.clDeclareDynVar;
pub const declareVar = c.clDeclareVar;
pub const expandTemplateType = c.clExpandTemplateType;
pub const setResolver = c.clSetResolver;
pub const resolve = c.clResolve;
pub const setModuleLoader = c.clSetModuleLoader;
pub const setModuleConfig = c.clSetModuleConfig;
pub const createModule = c.clCreateModule;
pub const getPrinter = c.clGetPrinter;
pub const setPrinter = c.clSetPrinter;
pub const getErrorPrinter = c.clGetErrorPrinter;
pub const setErrorPrinter = c.clSetErrorPrinter;
pub const setUserData = c.clSetUserData;
pub const getUserData = c.clGetUserData;
pub const getGlobalRC = c.clGetGlobalRC;
pub const countObjects = c.clCountObjects;
pub const newLastErrorSummary = c.clNewLastErrorSummary;
pub const newErrorReportSummary = c.clNewErrorReportSummary;
pub const newPanicSummary = c.clNewPanicSummary;
pub const release = c.clRelease;
pub const retain = c.clRetain;
pub const performGC = c.clPerformGC;
pub const traceDumpLiveObjects = c.clTraceDumpLiveObjects;
pub const resultName = c.clResultName;
pub const STR = c.CL_STR;
pub inline fn CORE_TYPE(type_id: TypeId) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_CORE_CUSTOM,
        .data = .{ .core_custom = .{
            .type_id = type_id,
            .get_children = null,
            .finalizer = null,
        }},
    };
}
pub inline fn CORE_TYPE_EXT(type_id: TypeId, get_children: GetChildrenFn, finalizer: FinalizerFn, load_all_methods: bool) HostType {
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
pub inline fn CORE_TYPE_DECL(type_id: TypeId) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_CORE_DECL,
        .data = .{ .core_decl = .{
            .type_id = type_id,
        }},
    };
}
pub inline fn DECL_TYPE(out_type_id: ?*TypeId) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_DECL,
        .data = .{ .decl = .{
            .out_type_id = out_type_id,
        }},
    };
}
pub inline fn CUSTOM_TYPE(out_type_id: ?*TypeId, get_children: GetChildrenFn, finalizer: FinalizerFn) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_CUSTOM,
        .data = .{ .custom = .{
            .out_type_id = out_type_id,
            .get_children = get_children,
            .finalizer = finalizer,
            .pre = false,
        }},
    };
}
pub inline fn CUSTOM_PRE_TYPE(out_type_id: ?*TypeId, get_children: GetChildrenFn, finalizer: FinalizerFn) HostType {
    return HostType{
        .type = c.CL_BIND_TYPE_CUSTOM,
        .data = .{ .custom = .{
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
pub const Sym = c.CLSym;
pub const GCResult = c.CLGCResult;

pub const GetChildrenFn = c.CLGetChildrenFn;
pub const FinalizerFn = c.CLFinalizerFn;

pub const FuncEnumType = enum(u8) {
    standard = c.CL_FUNC_STANDARD,
    inlinec = c.CL_FUNC_INLINE,
};

pub const BindTypeCustom = c.CL_BIND_TYPE_CUSTOM;
pub const BindTypeCoreCustom = c.CL_BIND_TYPE_CORE_CUSTOM;
pub const BindTypeCoreDecl = c.CL_BIND_TYPE_CORE_DECL;
pub const BindTypeDecl = c.CL_BIND_TYPE_DECL;

pub const Backend = c.CLBackend;
pub const BackendVM = c.CL_VM;
pub const BackendJIT = c.CL_JIT;
pub const BackendTCC = c.CL_TCC;
pub const BackendCC = c.CL_CC;
pub const BackendLLVM = c.CL_LLVM;

pub const TypeId = c.CLTypeId;
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
pub const TypeClosure = c.CL_TYPE_CLOSURE;
pub const TypeLambda = c.CL_TYPE_LAMBDA;
pub const TypeString = c.CL_TYPE_STRING;
pub const TypeArray = c.CL_TYPE_ARRAY;
pub const TypeFiber = c.CL_TYPE_FIBER;
pub const TypeBox = c.CL_TYPE_BOX;
pub const TypeHostFunc = c.CL_TYPE_HOST_FUNC;
pub const TypeExternFunc = c.CL_TYPE_EXTERN_FUNC;
pub const TypeTccState = c.CL_TYPE_TCCSTATE;
pub const TypePointer = c.CL_TYPE_POINTER;
pub const TypeTuple = c.CL_TYPE_TUPLE;
pub const TypeType = c.CL_TYPE_TYPE;
pub const TypeMetaType = c.CL_TYPE_METATYPE;

pub const ResultCode = c.CLResultCode;
pub const Success = c.CL_SUCCESS;
pub const Await = c.CL_AWAIT;
pub const ErrorCompile = c.CL_ERROR_COMPILE;
pub const ErrorPanic = c.CL_ERROR_PANIC;
pub const ErrorUnknown = c.CL_ERROR_UNKNOWN;