// Copyright (c) 2023 Cyber (See LICENSE)
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

#ifndef CYBER_H
#define CYBER_H

#ifdef __cplusplus
extern "C" {
#endif
typedef struct CLVM CLVM;

typedef uint64_t CLValue;
typedef struct CLValueSlice {
    const CLValue* ptr;
    size_t len;
} CLValueSlice;

typedef struct CLResolverParams CLResolverParams;
typedef struct CLModule CLModule;

#define CL_NULLID UINT32_MAX
#define CL_VOID 0x7FFC000100000000
#define CL_TRUE 0x7FFC000200000001
#define CL_FALSE 0x7FFC000200000000
#define CL_INTERRUPT 0x7ffc00030000ffff

typedef int CLResultCode;

enum {
    CL_SUCCESS = 0,
    CL_AWAIT,
    CL_ERROR_COMPILE,
    CL_ERROR_PANIC,
    CL_ERROR_UNKNOWN,
};

enum {
    CL_TYPE_NULL = 0,
    CL_TYPE_VOID,
    CL_TYPE_BOOLEAN,
    CL_TYPE_ERROR,
    CL_TYPE_BYTE,
    CL_TYPE_TAGLIT,
    CL_TYPE_SYMBOL,
    CL_TYPE_INTEGER,
    CL_TYPE_FLOAT,
    CL_TYPE_DYN,
    CL_TYPE_ANY,
    CL_TYPE_TYPE,
    CL_TYPE_TUPLE,
    CL_TYPE_LIST_DYN,
    CL_TYPE_LISTITER_DYN,
    CL_TYPE_MAP,
    CL_TYPE_MAPITER,
    CL_TYPE_FUNC,
    CL_TYPE_FUNC_SIG,
    CL_TYPE_PLACEHOLDER2,
    CL_TYPE_EXTERN_FUNC,
    CL_TYPE_STRING,
    CL_TYPE_EXPRTYPE,
    CL_TYPE_FIBER,
    CL_TYPE_UPVALUE,
    CL_TYPE_TCCSTATE,
    CL_TYPE_PLACEHOLDER3,
    CL_TYPE_RANGE,
    CL_TYPE_TABLE,
    CL_TYPE_MEMORY,
};
typedef uint32_t CLType;

typedef uint32_t CLFuncSigId;

// Cyber deals with string slices internally for efficiency.
// Some API functions may require you to use slices rather than a null terminated string.
// Creating a CLStr can be simplified with a macro:
// #define str(x) ((CLStr){ x, strlen(x) })
// NOTE: Returned `CLStr`s are not always null terminated.
typedef struct CLStr {
    const char* ptr;
    size_t len;
} CLStr;

typedef struct CLSlice {
    void* ptr;
    size_t len;
} CLSlice;

typedef struct CLSym {
    void* ptr;
} CLSym;

typedef struct CLNode {
    void* ptr;
} CLNode;

typedef struct CLModule {
    void* ptr;
} CLModule;

// @host func is binded to this function pointer signature.
typedef CLValue (*CLFuncFn)(CLVM*, const CLValue*, uint8_t);

typedef struct CLResolverParams {
    /// Chunk that invoked the resolver.
    /// If `CL_NULLID`, then the invoker did not originate from another chunk module.
    uint32_t chunkId;

    /// The current URI.
    /// If it's the empty string, then the invoker did not originate from another chunk module.
    CLStr curUri;

    /// The unresolved URI.
    CLStr uri;

    // Buffer to write a dynamic the result to.
    char* buf;
    size_t bufLen;

    // Result.
    const char** resUri;
    size_t* resUriLen;
} CLResolverParams;

// Given the current module's resolved URI and the "to be" imported module specifier,
// set `params.resUri`, `params.resUriLen`, and return true.
// If the result uri is dynamic, write to `params.buf` to build the uri and set `params.resUri` to `params.buf`.
// If the result uri is a shared reference, `params.buf` does not need to be used.
// Return false if the uri can not be resolved.
// Most embedders do not need a resolver and can rely on the default resolver which
// simply returns `params.spec` without any adjustments.
typedef bool (*CLResolverFn)(CLVM* vm, CLResolverParams params);

// Callback invoked after all type symbols in the module's src are loaded.
// This could be used to set up an array or hashmap for binding @host vars.
typedef void (*CLModuleOnTypeLoadFn)(CLVM* vm, CLSym mod);

// Callback invoked after all symbols in the module's src are loaded.
// This could be used to inject symbols not declared in the module's src.
typedef void (*CLModuleOnLoadFn)(CLVM* vm, CLSym mod);

// Callback invoked just before the module is destroyed.
// This could be used to cleanup (eg. release) injected symbols from `CLPostLoadModuleFn`,
typedef void (*CLModuleOnDestroyFn)(CLVM* vm, CLSym mod);

// Info about a @host func.
typedef struct CLFuncInfo {
    // The module it belongs to.
    CLSym mod;
    // The name of the func.
    CLStr name;
    // The function's signature.
    uint32_t funcSigId;
} CLFuncInfo;

// A mapping from a matching symbol string to a CLFuncFn.
typedef struct CLHostFuncEntry {
    CLStr name;
    CLFuncFn func;
} CLHostFuncEntry;

// Given info about a @host func, write it's function pointer to `out->ptr` and return true,
// or return false.
typedef bool (*CLFuncLoaderFn)(CLVM* vm, CLFuncInfo funcInfo, CLFuncFn* out);

// Info about a @host var.
typedef struct CLVarInfo {
    // The module it belongs to.
    CLSym mod;
    // The name of the var.
    CLStr name;
    // A counter that tracks it's current position among all @host vars in the module.
    // This is useful if you want to bind an array of `CLValue`s to @host vars.
    uint32_t idx;
} CLVarInfo;

// Given info about a @host var, write a value to `out` and return true, or return false.
// The value is consumed by the module. If the value should outlive the module,
// call `clRetain` before handing it over.
typedef bool (*CLVarLoaderFn)(CLVM* vm, CLVarInfo funcInfo, CLValue* out);

// Info about a @host type.
typedef struct CLTypeInfo {
    // The module it belongs to.
    CLSym mod;
    // The name of the type.
    CLStr name;
} CLTypeInfo;

typedef enum {
    // Create a new object type with it's own memory layout and finalizer.
    CL_BIND_TYPE_HOSTOBJ,

    // Use the provided declaration with a predefined or generated type id.
    CL_BIND_TYPE_DECL,

    // Create a new type that has a predefined type id.
    CL_BIND_TYPE_CORE_CUSTOM,

    CL_BIND_TYPE_CREATE,
} CLBindTypeKind;

typedef CLSym (*CLCreateTypeFn)(CLVM* vm, CLSym mod, CLNode decl);

// Given an object, return the pointer of an array and the number of children.
// If there are no children, return NULL and 0 for the length.
typedef CLValueSlice (*CLGetChildrenFn)(CLVM* vm, void* obj);

// Use the finalizer to perform cleanup tasks for the object (eg. free a resource handle)
// before it is finally freed by the VM.
//
// Unlike finalizers declared in user scripts, this finalizer is guaranteed to be invoked.
//
// NOTE: Although the VM handle is provided, using the VM at this point to mutate object dependencies
//       is undefined behavior because the VM may be running a GC task.
//
// NOTE: If the object retains child VM objects, accessing them is undefined behavior
//       because they could have freed before the finalizer was invoked.
typedef void (*CLFinalizerFn)(CLVM* vm, void* obj);

// Result given to Cyber when binding a @host type.
typedef struct CLHostType {
    union {
        struct {
            // The reserved `typeId` is used for the new type.
            CLType type_id;
            // Pointer to callback or null.
            CLGetChildrenFn get_children;
            // Pointer to callback or null.
            CLFinalizerFn finalizer;
            bool load_all_methods;
        } core_custom;
        struct {
            // If not null, the created runtime type id will be written to `outTypeId`.
            // This typeId is then used to allocate a new instance of the object.
            // Defaults to null.
            CLType* out_type_id;
            // If `true`, invokes finalizer before visiting children.
            bool pre;
            // Pointer to callback or null.
            CLGetChildrenFn get_children;
            // Pointer to callback or null.
            CLFinalizerFn finalizer;
        } hostobj;
        struct {
            // If `CL_NULLID`, a new type id is generated and written to a non-null `out_type_id`.
            CLType type_id;
            CLType* out_type_id;
        } decl;
        struct {
            // Pointer to callback.
            CLCreateTypeFn create_fn;
        } create;     
    } data;
    // `CLBindTypeKind`.
    uint8_t type;
} CLHostType;

#define CL_STR(str) ((CLStr){ .ptr = str, .len = strlen(str) })
#define CL_FUNC(name, fn) ((CLHostFuncEntry){ CL_STR(name), fn })
// #define CL_CORE_TYPE(t) ((CLHostType){ .data = { .core_custom = { .type_id = t, .get_children = NULL, .finalizer = NULL }}, .type = CL_BIND_TYPE_CORE_CUSTOM })
// #define CL_CORE_TYPE_EXT(t, gc, f) ((CLHostType){ .data = { .core_custom = { .type_id = t, .get_children = gc, .finalizer = f }}, .type = CL_BIND_TYPE_CORE_CUSTOM })
// #define CL_CORE_TYPE_DECL(t) ((CLHostType){ .data = { .core_decl = { .type_id = t }}, .type = CL_BIND_TYPE_CORE_DECL })
#define CL_CUSTOM_TYPE(name, ot, gc, f) ((CLHostTypeEntry){ CL_STR(name), (CLHostType){ .data = { .hostobj = { .out_type_id = ot, .pre = false, .get_children = gc, .finalizer = f }}, .type = CL_BIND_TYPE_HOSTOBJ }})
#define CL_CUSTOM_PRE_TYPE(name, ot, gc, f) ((CLHostTypeEntry){ CL_STR(name), (CLHostType){ .data = { .hostobj = { .out_type_id = ot, .pre = true, .get_children = gc, .finalizer = f }}, .type = CL_BIND_TYPE_HOSTOBJ }})

// A mapping from a matching symbol string to a CLHostType.
typedef struct CLHostTypeEntry {
    CLStr name;
    CLHostType host_t;
} CLHostTypeEntry;

// Optional callback if a host type could not be found in `CLModuleLoaderResult.types`.
// Given info about a @host type, write the result to `out` and return true, or return false.
typedef bool (*CLTypeLoaderFn)(CLVM* vm, CLTypeInfo typeInfo, CLHostType* out);

// Given the resolved import specifier of the module, set the module's src in `res->src`,
// set symbol loaders, and return true. Otherwise, return false.
typedef bool (*CLModuleLoaderFn)(CLVM* vm, CLStr resolved_uri, CLModule* res);

typedef struct CLModuleConfig {
    CLSlice funcs;                   // `CLHostFuncEntry` slice.
    CLFuncLoaderFn func_loader;      // Pointer to callback or null.
    CLVarLoaderFn varLoader;         // Pointer to callback or null.
    CLSlice types;                   // `CLHostTypeEntry` slice.
    CLTypeLoaderFn type_loader;      // Pointer to callback or null.
    CLModuleOnTypeLoadFn onTypeLoad; // Pointer to callback or null.
    CLModuleOnLoadFn onLoad;         // Pointer to callback or null.
    CLModuleOnDestroyFn onDestroy;   // Pointer to callback or null.
} CLModuleConfig;

// Handler for printing. The builtin `print` would invoke this.
// The default behavior is a no-op.
typedef void (*CLPrintFn)(CLVM* vm, CLStr str);

// Handler for printing errors.
// The default behavior is a no-op.
typedef void (*CLPrintErrorFn)(CLVM* vm, CLStr str);

// Handler for compiler and runtime logs.
// The default behavior is a no-op.
typedef void (*CLLogFn)(CLStr str);

typedef enum {
    CL_VM = 0,
    CL_JIT,
    CL_TCC,
    CL_CC,
    CL_LLVM,
} CLBackend;

typedef struct CLEvalConfig {
    /// Compiler options.
    bool single_run;
    bool file_modules;
    bool gen_all_debug_syms;
    CLBackend backend;

    /// Whether url imports and cached assets should be reloaded.
    /// TODO: This should be part of CLI config.
    bool reload;

    bool spawn_exe;
} CLEvalConfig;

typedef struct CLCompileConfig {
    /// Whether this process intends to perform eval once and exit.
    /// In that scenario, the compiler can skip generating the final release ops for the main block.
    bool single_run;

    bool skip_codegen;

    bool file_modules;

    /// By default, debug syms are only generated for insts that can potentially fail.
    bool gen_all_debug_syms;

    CLBackend backend;

    bool emit_source_map;

    bool gen_debug_func_markers;
} CLCompileConfig;

typedef struct CLValidateConfig {
    /// Compiler options.
    bool file_modules;
} CLValidateConfig;

typedef struct CLAllocator {
    void* ptr;
    const void* vtable;
} CLAllocator;

typedef struct CLFieldInit {
    CLStr name;
    CLValue value;
} CLFieldInit;

// -----------------------------------
// [ Top level ]
// -----------------------------------
CLStr clGetFullVersion(void);
CLStr clGetVersion(void);
CLStr clGetBuild(void);
CLStr clGetCommit(void);
CLStr clResultName(CLResultCode code);
extern CLLogFn clLog;

// -----------------------------------
// [ VM ]
// -----------------------------------

CLVM* clCreate(void);

// Deinitialize static objects so that reference counts can be verified.
// Afterwards, call `clDestroy` or perform a check on `clCountObjects`.
void clDeinit(CLVM* vm);

// Deinitializes the VM and frees all memory associated with it. Any operation on `vm` afterwards is undefined.
void clDestroy(CLVM* vm);

CLResolverFn clGetResolver(CLVM* vm);
void clSetResolver(CLVM* vm, CLResolverFn resolver);
CLStr clResolve(CLVM* vm, CLStr uri);

// The default module resolver. It returns `spec`.
bool clDefaultResolver(CLVM* vm, CLResolverParams params);

CLModuleLoaderFn clGetModuleLoader(CLVM* vm);
void clSetModuleLoader(CLVM* vm, CLModuleLoaderFn loader);

// The default module loader. It knows how to load the `builtins` module.
bool clDefaultModuleLoader(CLVM* vm, CLStr resolved_uri, CLModule* res);

CLPrintFn clGetPrinter(CLVM* vm);
void clSetPrinter(CLVM* vm, CLPrintFn print);
CLPrintErrorFn clGetErrorPrinter(CLVM* vm);
void clSetErrorPrinter(CLVM* vm, CLPrintErrorFn print);

CLEvalConfig clDefaultEvalConfig(void);
CLCompileConfig clDefaultCompileConfig(void);

// Resets the compiler and runtime state.
void clReset(CLVM* vm);

// Evalutes the source code and returns the result code.
// Subsequent evals will reuse the same compiler and runtime state.
// If the last statement of the script is an expression, `outVal` will contain the value.
CLResultCode clEval(CLVM* vm, CLStr src, CLValue* outVal);

// Accepts an eval config. Unlike `clEvalPath`, `uri` does not get resolved and only serves to identify `src` origin.
CLResultCode clEvalExt(CLVM* vm, CLStr uri, CLStr src, CLEvalConfig config, CLValue* outVal);

// Resolves `uri` and evaluates the module.
CLResultCode clEvalPath(CLVM* vm, CLStr uri, CLEvalConfig config, CLValue* outVal);

// Consumes and evaluates all ready tasks. Returns `CL_SUCCESS` if succesfully emptied the ready queue.
CLResultCode clRunReadyTasks(CLVM* vm);

CLResultCode clCompile(CLVM* vm, CLStr uri, CLStr src, CLCompileConfig config);
CLResultCode clValidate(CLVM* vm, CLStr src);

/// Get call arguments.
int64_t clGetInt(CLVM* vm, uint32_t idx);
double clGetFloat(CLVM* vm, uint32_t idx);
CLValue clGetValue(CLVM* vm, uint32_t idx);

/// Convenience function to return the last error summary.
/// Returns clNewErrorReportSummary if the last result was a CL_ERROR_COMPILE,
/// or clNewPanicSummary if the last result was a CL_ERROR_PANIC,
/// or the null string.
CLStr clNewLastErrorSummary(CLVM* vm);

/// Returns first compile-time report summary. Must be freed with `clFree`.
CLStr clNewErrorReportSummary(CLVM* vm);

/// Returns runtime panic summary. Must be freed with `clFree`.
CLStr clNewPanicSummary(CLVM* vm);

/// Some API callbacks use this to report errors.
void clReportApiError(CLVM* vm, CLStr msg);

// Attach a userdata pointer inside the VM.
void* clGetUserData(CLVM* vm);
void clSetUserData(CLVM* vm, void* userData);

void* clSetNewMockHttp(CLVM* vm);

// Verbose flag. In a debug build, this would print more logs.
extern bool clVerbose;

// Silent flag. Whether to print errors.
extern bool clSilent;

// -----------------------------------
// [ Modules ]
// -----------------------------------

CLModule clCreateModule(CLVM* vm, CLStr resolved_uri, CLStr src);
void clSetModuleConfig(CLVM* vm, CLModule mod, CLModuleConfig* config);

// -----------------------------------
// [ Symbols ]
// -----------------------------------

// Declares a function in a module.
void clDeclareFuncDyn(CLSym mod, const char* name, uint32_t numParams, CLFuncFn fn);
void clDeclareFunc(CLSym mod, const char* name, const CLType* params, size_t numParams, CLType retType, CLFuncFn fn);

// Declares a variable in a module.
void clDeclareDynVar(CLSym mod, const char* name, CLType type, CLValue val);
void clDeclareVar(CLSym mod, const char* name, CLType type, CLValue val);

// Expand type template for given arguments.
bool clExpandTemplateType(CLVM* vm, CLSym type_t, const CLValue* args, size_t nargs, CLType* res);

// Find and return the type from an absolute path.
// Returns `CL_TYPE_NULL` if the symbol could not be found or the symbol is not a type.
CLType clFindType(CLVM* vm, CLStr path);

// -----------------------------------
// [ Memory ]
// -----------------------------------
void clRelease(CLVM* vm, CLValue val);
void clRetain(CLVM* vm, CLValue val);

// Stats of a GC run.
typedef struct CLGCResult {
    // Objects freed that were part of a reference cycle.
    uint32_t numCycFreed;

    // Total number of objects freed.
    // NOTE: This is only available if built with `trace` enabled.
    uint32_t numObjFreed;
} CLGCResult;

// Run the reference cycle detector once and return statistics.
CLGCResult clPerformGC(CLVM* vm);

// Get's the current global reference count.
// NOTE: This will panic if the lib was not built with `TrackGlobalRC`.
// RELATED: `clCountObjects()`
size_t clGetGlobalRC(CLVM* vm);

// Returns the number of live objects.
// This can be used to check if all objects were cleaned up after `clDeinit`.
size_t clCountObjects(CLVM* vm);

// TRACE mode: Dump live objects recorded in global object map.
void clTraceDumpLiveObjects(CLVM* vm);

// For embedded, Cyber by default uses malloc (it can be configured to use the high-perf mimalloc).
// If the host uses a different allocator than Cyber, use `clAlloc` to allocate memory
// that is handed over to Cyber so it knows how to free it.
// This is also used to manage accessible buffers when embedding WASM.
// Only pointer is returned so wasm callsite can just receive the return value.
void* clAlloc(CLVM* vm, size_t size);

// When using the Zig allocator, you'll need to pass the original memory size.
// For all other allocators, use 1 for `len`.
void clFree(CLVM* vm, CLStr bytes);
void clFreeZ(CLVM* vm, const char* str);

CLAllocator clGetAllocator(CLVM* vm);

// -----------------------------------
// [ Values ]
//
// Functions that begin with `clNew` instantiate objects on the heap and
// automatically retain +1 refcount.
// -----------------------------------

CLStr clNewValueDump(CLVM* vm, CLValue val);

// Booleans.
CLValue clBool(bool b);
bool clAsBool(CLValue val); // Assumes boolean.
bool clToBool(CLValue val); // Performs a naive conversion.

// 64-bit integer.
CLValue clInt(int64_t n);               // Unboxed.
CLValue clNewInt(CLVM* vm, int64_t n);  // Boxed.
int64_t clAsBoxInt(CLValue val);        // From boxed.

// CLValue clInt32(int32_t n); // Unboxed.

// 64-bit float.
CLValue clFloat(double f);
double clAsFloat(CLValue val);

// Symbols.
CLValue clSymbol(CLVM* vm, CLStr str);
uint32_t clAsSymbolId(CLValue val);

// `clNewString` is the recommended way to create a new string.
// Use `Astring` or `Ustring` if you know it will be ASCII or UTF-8 respectively.
CLValue clNewString(CLVM* vm, CLStr str);
CLValue clNewAstring(CLVM* vm, CLStr str);
CLValue clNewUstring(CLVM* vm, CLStr str, uint32_t charLen);
CLStr clAsString(CLValue val);
CLStr clToTempString(CLVM* vm, CLValue val);  // Conversion from value to a basic string description.

CLValue clNewTuple(CLVM* vm, const CLValue* vals, size_t len);

// Functions.
CLValue clNewFuncDyn(CLVM* vm, uint32_t numParams, CLFuncFn func);
CLValue clNewFunc(CLVM* vm, const CLType* params, size_t numParams, CLType retType, CLFuncFn func);

// Pointers.
CLValue clNewPointerVoid(CLVM* vm, void* ptr);

// Types.
CLValue clNewType(CLVM* vm, CLType type_id);
CLType clGetType(CLValue val);

// Instantiating a `@host type` requires the `typeId` obtained from `CLTypeLoader` and
// the number of bytes the object will occupy. Objects of the same type can have different sizes.
// A `CLValue` which contains the object pointer is returned. Call `clAsHostObject` to obtain the pointer
// or use `clNewHostObjectPtr` to instantiate instead.
CLValue clNewHostObject(CLVM* vm, CLType typeId, size_t n);
void* clAsHostObject(CLValue val);

// Like `clNewHostObject` but returns the object's pointer. Wrap it into a value with `clHostObject`.
void* clNewHostObjectPtr(CLVM* vm, CLType typeId, size_t n);

// Returns a new instance of `type` with a list of field initializers.
CLValue clNewInstance(CLVM* vm, CLType type, const CLFieldInit* fields, size_t nfields);

// Returns the field value of the receiver object `rec`.
CLValue clGetField(CLVM* vm, CLValue rec, CLStr name);

// Re-encode pointer into a boxed heap object.
CLValue clHostObject(void* ptr);
CLValue clVmObject(void* ptr);

// Returns a new choice of a given case name and value.
CLValue clNewChoice(CLVM* vm, CLType choice_t, CLStr name, CLValue val);

// Returns the unwrapped value of a choice.
CLValue clUnwrapChoice(CLVM* vm, CLValue choice, CLStr name);

bool clIsFuture(CLVM* vm, CLValue val);

// List[T], `list_t` should be obtained from `clExpandTemplateType`.
// TODO: It should also be possible to obtain `list_t` from `clFindType("List[T]")`.
CLValue clNewEmptyList(CLVM* vm, CLType list_t);
CLValue clNewList(CLVM* vm, CLType list_t, const CLValue* vals, size_t len);

// List ops.
size_t clListLen(CLValue list);
size_t clListCap(CLValue list);
CLValue clListGet(CLVM* vm, CLValue list, size_t idx);
void clListSet(CLVM* vm, CLValue list, size_t idx, CLValue val);
void clListAppend(CLVM* vm, CLValue list, CLValue val);
void clListInsert(CLVM* vm, CLValue list, size_t idx, CLValue val);

CLValue clNewEmptyMap(CLVM* vm);

// Map ops.
// size_t clMapSize(CLValue map);
// bool clMapContains(CLValue map, CLValue key);
// bool clMapContainsStringKey(CLValue map, CLStr key);
// CLValue clMapGet(CLVM* vm, CLValue map, CLValue key);
// CLValue clMapGetStringKey(CLVM* vm, CLValue map, CLStr key);
// void clMapSet(CLVM* vm, CLValue map, CLValue key, CLValue val);
// void clMapSetStringKey(CLVM* vm, CLValue map, CLStr key, CLValue val);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif // !CYBER_H