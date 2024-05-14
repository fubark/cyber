// Copyright (c) 2023 Cyber (See LICENSE)
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

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
typedef struct CLModuleLoaderResult CLModuleLoaderResult;
typedef struct CLModule CLModule;

#define CL_NULLID UINT32_MAX

typedef int CLResultCode;

enum {
    CL_SUCCESS = 0,
    CL_ERROR_COMPILE,
    CL_ERROR_PANIC,
    CL_ERROR_UNKNOWN,
};

typedef enum {
    CL_TYPE_VOID = 0,
    CL_TYPE_BOOLEAN,
    CL_TYPE_ERROR,
    CL_TYPE_PLACEHOLDER1,
    CL_TYPE_PLACEHOLDER2,
    CL_TYPE_PLACEHOLDER3,
    CL_TYPE_SYMBOL,
    CL_TYPE_INTEGER,
    CL_TYPE_FLOAT,
    CL_TYPE_DYNAMIC,
    CL_TYPE_ANY,
    CL_TYPE_TYPE,
    CL_TYPE_TUPLE,
    CL_TYPE_LIST,
    CL_TYPE_LISTITER,
    CL_TYPE_MAP,
    CL_TYPE_MAPITER,
    CL_TYPE_CLOSURE,
    CL_TYPE_LAMBDA,
    CL_TYPE_HOST_FUNC,
    CL_TYPE_EXTERN_FUNC,
    CL_TYPE_STRING,
    CL_TYPE_ARRAY,
    CL_TYPE_FIBER,
    CL_TYPE_BOX,
    CL_TYPE_TCCSTATE,
    CL_TYPE_POINTER,
    CL_TYPE_METATYPE,
    CL_TYPE_RANGE,
    CL_TYPE_TABLE,
} CLType;
typedef uint32_t CLTypeId;

// Cyber deals with string slices internally for efficiency.
// Some API functions may require you to use slices rather than a null terminated string.
// Creating a CLStr can be simplified with a macro:
// #define str(x) ((CLStr){ x, strlen(x) })
// NOTE: Returned `CLStr`s are not always null terminated.
typedef struct CLStr {
    const char* ptr;
    size_t len;
} CLStr;

typedef CLStr CLSlice;

typedef struct CLSym {
    void* ptr;
} CLSym;

// @host func is binded to this function pointer signature.
typedef CLValue (*CLFuncFn)(CLVM* vm, const CLValue* args, uint8_t nargs);

// Internal @host func used to do inline caching.
typedef void (*CLInlineFuncFn)(CLVM* vm, uint8_t* pc, const CLValue* args, uint8_t nargs);

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
    // A counter that tracks it's current position among all @host funcs in the module.
    // This is useful if you want to bind an array of function pointers to @host funcs.
    uint32_t idx;
} CLFuncInfo;

// Result given to Cyber when binding a @host func.
typedef struct CLFuncResult {
    // Pointer to the binded function. (CLFuncFn)
    const void* ptr;
} CLFuncResult;

// Given info about a @host func, write it's function pointer to `out->ptr` and return true,
// or return false.
typedef bool (*CLFuncLoaderFn)(CLVM* vm, CLFuncInfo funcInfo, CLFuncResult* out);

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
    // A counter that tracks it's current position among all @host types in the module.
    // This is useful if you want to bind an array of data to @host types.
    uint32_t idx;
} CLTypeInfo;

typedef enum {
    // Create a new object type with it's own memory layout and finalizer.
    CL_BIND_TYPE_CUSTOM,

    // Create a new type from the given type declaration with a predefined type id.
    CL_BIND_TYPE_DECL,
} CLBindTypeKind;

// Given an object, return the pointer of an array and the number of children.
// If there are no children, return NULL and 0 for the length.
typedef CLValueSlice (*CLObjectGetChildrenFn)(CLVM* vm, void* obj);

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
typedef void (*CLObjectFinalizerFn)(CLVM* vm, void* obj);

// Result given to Cyber when binding a @host type.
typedef struct CLTypeResult {
    union {
        struct {
            // If not null, the created runtime type id will be written to `outTypeId`.
            // This typeId is then used to allocate a new instance of the object.
            // Defaults to null.
            CLTypeId* out_type_id;

            // If NullId, the next type id is consumed and written to `out_type_id`.
            CLTypeId type_id;

            // Pointer to callback or null.
            CLObjectGetChildrenFn get_children;
            // Pointer to callback or null.
            CLObjectFinalizerFn finalizer;
        } custom;
        struct {
            // The reserved `typeId` is used for the new type.
            CLTypeId type_id;
        } decl;
    } data;
    // `CLBindTypeKind`. By default, this is `CL_BIND_TYPE_CUSTOM`.
    uint8_t type;
} CLTypeResult;

// Given info about a @host type, write the result to `out` and return true, or return false.
typedef bool (*CLTypeLoaderFn)(CLVM* vm, CLTypeInfo typeInfo, CLTypeResult* out);

// This callback is invoked after receiving the module loader's result.
// If `res->src` was allocated, this can be a good time to free the memory.
typedef void (*CLModuleOnReceiptFn)(CLVM* vm, CLModuleLoaderResult* res);

// Module loader config.
typedef struct CLModuleLoaderResult {
    // The Cyber source code for the module.
    const char* src;
    size_t srcLen;

    CLModuleOnReceiptFn onReceipt;   // Pointer to callback or null.
    CLFuncLoaderFn funcLoader;       // Pointer to callback or null.
    CLVarLoaderFn varLoader;         // Pointer to callback or null.
    CLTypeLoaderFn typeLoader;       // Pointer to callback or null.
    CLModuleOnTypeLoadFn onTypeLoad; // Pointer to callback or null.
    CLModuleOnLoadFn onLoad;         // Pointer to callback or null.
    CLModuleOnDestroyFn onDestroy;   // Pointer to callback or null.
} CLModuleLoaderResult;

// Given the resolved import specifier of the module, set the module's src in `res->src`,
// set symbol loaders, and return true. Otherwise, return false.
typedef bool (*CLModuleLoaderFn)(CLVM* vm, CLStr resolvedSpec, CLModuleLoaderResult* out);

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

// -----------------------------------
// [ Top level ]
// -----------------------------------
CLStr clGetFullVersion(void);
CLStr clGetVersion(void);
CLStr clGetBuild(void);
CLStr clGetCommit(void);
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
bool clDefaultModuleLoader(CLVM* vm, CLStr resolvedSpec, CLModuleLoaderResult* out);

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

CLResultCode clCompile(CLVM* vm, CLStr uri, CLStr src, CLCompileConfig config);
CLResultCode clValidate(CLVM* vm, CLStr src);

/// Convenience function to return the last error summary.
/// Returns clNewErrorReportSummary if the last result was a CL_ERROR_COMPILE,
/// or clNewPanicSummary if the last result was a CL_ERROR_PANIC,
/// or the null string.
CLStr clNewLastErrorSummary(CLVM* vm);

/// Returns first compile-time report summary. Must be freed with `clFreeStr`.
CLStr clNewErrorReportSummary(CLVM* vm);

/// Returns runtime panic summary. Must be freed with `clFreeStr`.
CLStr clNewPanicSummary(CLVM* vm);

/// Some API callbacks use this to report errors.
void clReportApiError(CLVM* vm, CLStr msg);

// Attach a userdata pointer inside the VM.
void* clGetUserData(CLVM* vm);
void clSetUserData(CLVM* vm, void* userData);

CLStr clResultName(CLResultCode code);

// Verbose flag. In a debug build, this would print more logs.
extern bool clVerbose;

// Silent flag. Whether to print errors.
extern bool clSilent;

// -----------------------------------
// [ Symbols ]
// -----------------------------------

// Declares a function in a module.
void clDeclareDynFunc(CLSym mod, const char* name, uint32_t numParams, CLFuncFn fn);
void clDeclareFunc(CLSym mod, const char* name, const CLTypeId* params, size_t numParams, CLTypeId retType, CLFuncFn fn);

// Declares a variable in a module.
void clDeclareDynVar(CLSym mod, const char* name, CLTypeId type, CLValue val);
void clDeclareVar(CLSym mod, const char* name, CLTypeId type, CLValue val);

// Expand type template for given arguments.
CLTypeId clExpandTypeTemplate(CLSym type_t, CLValue* args, uint32_t nargs);

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
void clFree(CLVM* vm, CLSlice slice);
void clFreeStr(CLVM* vm, CLStr str);
void clFreeStrZ(CLVM* vm, const char* str);

CLAllocator clGetAllocator(CLVM* vm);

// -----------------------------------
// [ Values ]
//
// Functions that begin with `clNew` instantiate objects on the heap and
// automatically retain +1 refcount.
// -----------------------------------

// Create values.
CLValue clTrue(void);
CLValue clFalse(void);
CLValue clBool(bool b);

// int64_t is downcasted to a 48-bit int.
CLValue clInteger(int64_t n);
CLValue clInteger32(int32_t n);
CLValue clFloat(double f);
CLValue clHostObject(void* ptr);
CLValue clVmObject(void* ptr);
CLValue clSymbol(CLVM* vm, CLStr str);

// `clNewString` is the recommended way to create a new string. Use `Astring` or `Ustring` if you know it
// will be ASCII or UTF-8 respectively.
CLValue clNewString(CLVM* vm, CLStr str);
CLValue clNewAstring(CLVM* vm, CLStr str);
CLValue clNewUstring(CLVM* vm, CLStr str, uint32_t charLen);

CLValue clNewTuple(CLVM* vm, const CLValue* vals, size_t len);
CLValue clNewEmptyList(CLVM* vm);
CLValue clNewList(CLVM* vm, const CLValue* vals, size_t len);
CLValue clNewEmptyMap(CLVM* vm);
CLValue clNewUntypedFunc(CLVM* vm, uint32_t numParams, CLFuncFn func);
CLValue clNewFunc(CLVM* vm, const CLTypeId* params, uint32_t numParams, CLTypeId retType, CLFuncFn func);
CLValue clNewPointer(CLVM* vm, void* ptr);
CLValue clNewType(CLVM* vm, CLTypeId type_id);

// Instantiating a `@host type` requires the `typeId` obtained from `CLTypeLoader` and
// the number of bytes the object will occupy. Objects of the same type can have different sizes.
// A `CLValue` which contains the object pointer is returned. Call `clAsHostObject` to obtain the pointer
// or use `clNewHostObjectPtr` to instantiate instead.
CLValue clNewHostObject(CLVM* vm, CLTypeId typeId, size_t n);

// Like `clNewHostObject` but returns the object's pointer. Wrap it into a value with `clHostObject`.
void* clNewHostObjectPtr(CLVM* vm, CLTypeId typeId, size_t n);

// Instantiates a standard object type.
CLValue clNewVmObject(CLVM* vm, CLTypeId typeId);

// Values.
CLTypeId clGetTypeId(CLValue val);
CLStr clNewValueDump(CLVM* vm, CLValue val);

// Values to C.
double clAsFloat(CLValue val);
bool clToBool(CLValue val);
bool clAsBool(CLValue val);
int64_t clAsInteger(CLValue val);
uint32_t clAsSymbolId(CLValue val);
CLStr clToTempString(CLVM* vm, CLValue val);
CLStr clToTempRawString(CLVM* vm, CLValue val);
void* clAsHostObject(CLValue val);

// Lists.
size_t clListLen(CLValue list);
size_t clListCap(CLValue list);
CLValue clListGet(CLVM* vm, CLValue list, size_t idx);
void clListSet(CLVM* vm, CLValue list, size_t idx, CLValue val);
void clListAppend(CLVM* vm, CLValue list, CLValue val);
void clListInsert(CLVM* vm, CLValue list, size_t idx, CLValue val);

// Maps.
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