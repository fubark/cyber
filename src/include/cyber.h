// Copyright (c) 2023 Cyber (See LICENSE)
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#ifndef CYBER_H
#define CYBER_H

#ifdef __cplusplus
extern "C" {
#endif
typedef struct CsVM CsVM;

typedef uint64_t CsValue;
typedef struct CsValueSlice {
    const CsValue* ptr;
    size_t len;
} CsValueSlice;

typedef struct CsResolverParams CsResolverParams;
typedef struct CsModuleLoaderResult CsModuleLoaderResult;
typedef struct CsModule CsModule;

#define CS_NULLID UINT32_MAX

typedef int CsResultCode;

enum {
    CS_SUCCESS = 0,
    CS_ERROR_COMPILE,
    CS_ERROR_PANIC,
    CS_ERROR_UNKNOWN,
};

typedef enum {
    CS_TYPE_VOID = 0,
    CS_TYPE_BOOLEAN,
    CS_TYPE_ERROR,
    CS_TYPE_PLACEHOLDER1,
    CS_TYPE_PLACEHOLDER2,
    CS_TYPE_PLACEHOLDER3,
    CS_TYPE_SYMBOL,
    CS_TYPE_INTEGER,
    CS_TYPE_FLOAT,
    CS_TYPE_DYNAMIC,
    CS_TYPE_ANY,
    CS_TYPE_TYPE,
    CS_TYPE_TUPLE,
    CS_TYPE_LIST,
    CS_TYPE_LISTITER,
    CS_TYPE_MAP,
    CS_TYPE_MAPITER,
    CS_TYPE_CLOSURE,
    CS_TYPE_LAMBDA,
    CS_TYPE_HOST_FUNC,
    CS_TYPE_EXTERN_FUNC,
    CS_TYPE_STRING,
    CS_TYPE_ARRAY,
    CS_TYPE_FIBER,
    CS_TYPE_BOX,
    CS_TYPE_TCCSTATE,
    CS_TYPE_POINTER,
    CS_TYPE_METATYPE,
    CS_TYPE_RANGE,
    CS_TYPE_TABLE,
} CsType;
typedef uint32_t CsTypeId;

// Cyber deals with string slices internally for efficiency.
// Some API functions may require you to use slices rather than a null terminated string.
// Creating a CsStr can be simplified with a macro:
// #define str(x) ((CsStr){ x, strlen(x) })
// NOTE: Returned `CsStr`s are not always null terminated.
typedef struct CsStr {
    const char* buf;
    size_t len;
} CsStr;

typedef CsStr CsSlice;

typedef struct CsSym {
    void* ptr;
} CsSym;

// #host func is binded to this function pointer signature.
typedef CsValue (*CsFuncFn)(CsVM* vm, const CsValue* args, uint8_t nargs);

// Internal #host func used to do inline caching.
typedef void (*CsInlineFuncFn)(CsVM* vm, uint8_t* pc, const CsValue* args, uint8_t nargs);

typedef struct CsResolverParams {
    uint32_t chunkId;
    CsStr curUri;
    CsStr spec;

    // Buffer to write a dynamic the result to.
    char* buf;
    size_t bufLen;

    // Result.
    const char** resUri;
    size_t* resUriLen;
} CsResolverParams;

// Given the current module's resolved URI and the "to be" imported module specifier,
// set `params.resUri`, `params.resUriLen`, and return true.
// If the result uri is dynamic, write to `params.buf` to build the uri and set `params.resUri` to `params.buf`.
// If the result uri is a shared reference, `params.buf` does not need to be used.
// Return false if the uri can not be resolved.
// Most embedders do not need a resolver and can rely on the default resolver which
// simply returns `params.spec` without any adjustments.
typedef bool (*CsResolverFn)(CsVM* vm, CsResolverParams params);

// Callback invoked after all type symbols in the module's src are loaded.
// This could be used to set up an array or hashmap for binding #host vars.
typedef void (*CsModuleOnTypeLoadFn)(CsVM* vm, CsSym mod);

// Callback invoked after all symbols in the module's src are loaded.
// This could be used to inject symbols not declared in the module's src.
typedef void (*CsModuleOnLoadFn)(CsVM* vm, CsSym mod);

// Callback invoked just before the module is destroyed.
// This could be used to cleanup (eg. release) injected symbols from `CsPostLoadModuleFn`,
typedef void (*CsModuleOnDestroyFn)(CsVM* vm, CsSym mod);

// Info about a #host func.
typedef struct CsFuncInfo {
    // The module it belongs to.
    CsSym mod;
    // The name of the func.
    CsStr name;
    // The function's signature.
    uint32_t funcSigId;
    // A counter that tracks it's current position among all #host funcs in the module.
    // This is useful if you want to bind an array of function pointers to #host funcs.
    uint32_t idx;
} CsFuncInfo;

// Result given to Cyber when binding a #host func.
typedef struct CsFuncResult {
    // Pointer to the binded function. (CsFuncFn)
    const void* ptr;
} CsFuncResult;

// Given info about a #host func, write it's function pointer to `out->ptr` and return true,
// or return false.
typedef bool (*CsFuncLoaderFn)(CsVM* vm, CsFuncInfo funcInfo, CsFuncResult* out);

// Info about a #host var.
typedef struct CsVarInfo {
    // The module it belongs to.
    CsSym mod;
    // The name of the var.
    CsStr name;
    // A counter that tracks it's current position among all #host vars in the module.
    // This is useful if you want to bind an array of `CsValue`s to #host vars.
    uint32_t idx;
} CsVarInfo;

// Given info about a #host var, write a value to `out` and return true, or return false.
// The value is consumed by the module. If the value should outlive the module,
// call `csRetain` before handing it over.
typedef bool (*CsVarLoaderFn)(CsVM* vm, CsVarInfo funcInfo, CsValue* out);

// Info about a #host type.
typedef struct CsTypeInfo {
    // The module it belongs to.
    CsSym mod;
    // The name of the type.
    CsStr name;
    // A counter that tracks it's current position among all #host types in the module.
    // This is useful if you want to bind an array of data to #host types.
    uint32_t idx;
} CsTypeInfo;

typedef enum {
    // Create a new object type with it's own memory layout and finalizer.
    CS_BIND_TYPE_CUSTOM,

    // Create a new type from the given type declaration with a predefined type id.
    CS_BIND_TYPE_DECL,
} CsBindTypeKind;

// Given an object, return the pointer of an array and the number of children.
// If there are no children, return NULL and 0 for the length.
typedef CsValueSlice (*CsObjectGetChildrenFn)(CsVM* vm, void* obj);

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
typedef void (*CsObjectFinalizerFn)(CsVM* vm, void* obj);

// Result given to Cyber when binding a #host type.
typedef struct CsTypeResult {
    union {
        struct {
            // If not null, the created runtime type id will be written to `outTypeId`.
            // This typeId is then used to allocate a new instance of the object.
            // Defaults to null.
            CsTypeId* out_type_id;

            // If NullId, the next type id is consumed and written to `out_type_id`.
            CsTypeId type_id;

            // Pointer to callback or null.
            CsObjectGetChildrenFn get_children;
            // Pointer to callback or null.
            CsObjectFinalizerFn finalizer;
        } custom;
        struct {
            // The reserved `typeId` is used for the new type.
            CsTypeId type_id;
        } decl;
    } data;
    // `CsBindTypeKind`. By default, this is `CS_BIND_TYPE_CUSTOM`.
    uint8_t type;
} CsTypeResult;

// Given info about a #host type, write the result to `out` and return true, or return false.
typedef bool (*CsTypeLoaderFn)(CsVM* vm, CsTypeInfo typeInfo, CsTypeResult* out);

// This callback is invoked after receiving the module loader's result.
// If `res->src` was allocated, this can be a good time to free the memory.
typedef void (*CsModuleOnReceiptFn)(CsVM* vm, CsModuleLoaderResult* res);

// Module loader config.
typedef struct CsModuleLoaderResult {
    // The Cyber source code for the module.
    const char* src;

    // If `src` is not null terminated, `srcLen` needs to be set.
    // Defaults to 0 which indicates that `src` is null terminated.
    size_t srcLen;

    CsModuleOnReceiptFn onReceipt;   // Pointer to callback or null.
    CsFuncLoaderFn funcLoader;       // Pointer to callback or null.
    CsVarLoaderFn varLoader;         // Pointer to callback or null.
    CsTypeLoaderFn typeLoader;       // Pointer to callback or null.
    CsModuleOnTypeLoadFn onTypeLoad; // Pointer to callback or null.
    CsModuleOnLoadFn onLoad;         // Pointer to callback or null.
    CsModuleOnDestroyFn onDestroy;   // Pointer to callback or null.
} CsModuleLoaderResult;

// Given the resolved import specifier of the module, set the module's src in `res->src`,
// set symbol loaders, and return true. Otherwise, return false.
typedef bool (*CsModuleLoaderFn)(CsVM* vm, CsStr resolvedSpec, CsModuleLoaderResult* out);

// Handler for printing. The builtin `print` would invoke this.
// The default behavior is a no-op.
typedef void (*CsPrintFn)(CsVM* vm, CsStr str);

// Handler for printing errors.
// The default behavior is a no-op.
typedef void (*CsPrintErrorFn)(CsVM* vm, CsStr str);

// Handler for compiler and runtime logs.
// The default behavior is a no-op.
typedef void (*CsLogFn)(CsStr str);

typedef enum {
    CS_VM = 0,
    CS_JIT,
    CS_TCC,
    CS_CC,
    CS_LLVM,
} CsBackend;

typedef struct CsEvalConfig {
    /// Compiler options.
    bool single_run;
    bool file_modules;
    bool gen_all_debug_syms;
    CsBackend backend;

    /// Whether url imports and cached assets should be reloaded.
    /// TODO: This should be part of CLI config.
    bool reload;

    bool spawn_exe;
} CsEvalConfig;

typedef struct CsCompileConfig {
    /// Whether this process intends to perform eval once and exit.
    /// In that scenario, the compiler can skip generating the final release ops for the main block.
    bool single_run;

    bool skip_codegen;

    bool file_modules;

    /// By default, debug syms are only generated for insts that can potentially fail.
    bool gen_all_debug_syms;

    CsBackend backend;

    bool emit_source_map;

    bool gen_debug_func_markers;
} CsCompileConfig;

typedef struct CsValidateConfig {
    /// Compiler options.
    bool file_modules;
} CsValidateConfig;

typedef struct CsAllocator {
    void* ptr;
    const void* vtable;
} CsAllocator;

// -----------------------------------
// [ Top level ]
// -----------------------------------
CsStr csGetFullVersion(void);
CsStr csGetVersion(void);
CsStr csGetBuild(void);
CsStr csGetCommit(void);
extern CsLogFn csLog;

// -----------------------------------
// [ VM ]
// -----------------------------------

CsVM* csCreate(void);

// Deinitialize static objects so that reference counts can be verified.
// Afterwards, call `csDestroy` or perform a check on `csCountObjects`.
void csDeinit(CsVM* vm);

// Deinitializes the VM and frees all memory associated with it. Any operation on `vm` afterwards is undefined.
void csDestroy(CsVM* vm);

CsResolverFn csGetResolver(CsVM* vm);
void csSetResolver(CsVM* vm, CsResolverFn resolver);

// The default module resolver. It returns `spec`.
bool csDefaultResolver(CsVM* vm, CsResolverParams params);

CsModuleLoaderFn csGetModuleLoader(CsVM* vm);
void csSetModuleLoader(CsVM* vm, CsModuleLoaderFn loader);

// The default module loader. It knows how to load the `builtins` module.
bool csDefaultModuleLoader(CsVM* vm, CsStr resolvedSpec, CsModuleLoaderResult* out);

CsPrintFn csGetPrinter(CsVM* vm);
void csSetPrinter(CsVM* vm, CsPrintFn print);
CsPrintErrorFn csGetErrorPrinter(CsVM* vm);
void csSetErrorPrinter(CsVM* vm, CsPrintErrorFn print);

CsEvalConfig csDefaultEvalConfig(void);
CsCompileConfig csDefaultCompileConfig(void);

// Evalutes the source code and returns the result code.
// If the last statement of the script is an expression, `outVal` will contain the value.
CsResultCode csEval(CsVM* vm, CsStr src, CsValue* outVal);
CsResultCode csEvalExt(CsVM* vm, CsStr uri, CsStr src, CsEvalConfig config, CsValue* outVal);
CsResultCode csCompile(CsVM* vm, CsStr uri, CsStr src, CsCompileConfig config);
CsResultCode csValidate(CsVM* vm, CsStr src);

/// Convenience function to return the last error summary.
/// Returns csNewErrorReportSummary if the last result was a CS_ERROR_COMPILE,
/// or csNewPanicSummary if the last result was a CS_ERROR_PANIC,
/// or the null string.
CsStr csNewLastErrorSummary(CsVM* vm);

/// Returns first compile-time report summary. Must be freed with `csFreeStr`.
CsStr csNewErrorReportSummary(CsVM* vm);

/// Returns runtime panic summary. Must be freed with `csFreeStr`.
CsStr csNewPanicSummary(CsVM* vm);

/// Some API callbacks use this to report errors.
void csReportApiError(CsVM* vm, CsStr msg);

// Attach a userdata pointer inside the VM.
void* csGetUserData(CsVM* vm);
void csSetUserData(CsVM* vm, void* userData);

CsStr csResultName(CsResultCode code);

// Verbose flag. In a debug build, this would print more logs.
extern bool csVerbose;

// Silent flag. Whether to print errors.
extern bool csSilent;

// -----------------------------------
// [ Symbols ]
// -----------------------------------

// Declares a function in a module.
void csDeclareDynFunc(CsSym mod, const char* name, uint32_t numParams, CsFuncFn fn);
void csDeclareFunc(CsSym mod, const char* name, const CsTypeId* params, size_t numParams, CsTypeId retType, CsFuncFn fn);

// Declares a variable in a module.
void csDeclareDynVar(CsSym mod, const char* name, CsTypeId type, CsValue val);
void csDeclareVar(CsSym mod, const char* name, CsTypeId type, CsValue val);

// Expand type template for given arguments.
CsTypeId csExpandTypeTemplate(CsSym type_t, CsValue* args, uint32_t nargs);

// -----------------------------------
// [ Memory ]
// -----------------------------------
void csRelease(CsVM* vm, CsValue val);
void csRetain(CsVM* vm, CsValue val);

// Stats of a GC run.
typedef struct CsGCResult {
    // Objects freed that were part of a reference cycle.
    uint32_t numCycFreed;

    // Total number of objects freed.
    // NOTE: This is only available if built with `trace` enabled.
    uint32_t numObjFreed;
} CsGCResult;

// Run the reference cycle detector once and return statistics.
CsGCResult csPerformGC(CsVM* vm);

// Get's the current global reference count.
// NOTE: This will panic if the lib was not built with `TrackGlobalRC`.
// RELATED: `csCountObjects()`
size_t csGetGlobalRC(CsVM* vm);

// Returns the number of live objects.
// This can be used to check if all objects were cleaned up after `csDeinit`.
size_t csCountObjects(CsVM* vm);

// TRACE mode: Dump live objects recorded in global object map.
void csTraceDumpLiveObjects(CsVM* vm);

// For embedded, Cyber by default uses malloc (it can be configured to use the high-perf mimalloc).
// If the host uses a different allocator than Cyber, use `csAlloc` to allocate memory
// that is handed over to Cyber so it knows how to free it.
// This is also used to manage accessible buffers when embedding WASM.
// Only pointer is returned so wasm callsite can just receive the return value.
void* csAlloc(CsVM* vm, size_t size);

// When using the Zig allocator, you'll need to pass the original memory size.
// For all other allocators, use 1 for `len`.
void csFree(CsVM* vm, CsSlice slice);
void csFreeStr(CsVM* vm, CsStr str);
void csFreeStrZ(CsVM* vm, const char* str);

CsAllocator csGetAllocator(CsVM* vm);

// -----------------------------------
// [ Values ]
//
// Functions that begin with `csNew` instantiate objects on the heap and
// automatically retain +1 refcount.
// -----------------------------------

// Create values.
CsValue csTrue(void);
CsValue csFalse(void);
CsValue csBool(bool b);

// int64_t is downcasted to a 48-bit int.
CsValue csInteger(int64_t n);
CsValue csInteger32(int32_t n);
CsValue csFloat(double f);
CsValue csHostObject(void* ptr);
CsValue csVmObject(void* ptr);
CsValue csSymbol(CsVM* vm, CsStr str);

// `csNewString` is the recommended way to create a new string. Use `Astring` or `Ustring` if you know it
// will be ASCII or UTF-8 respectively.
CsValue csNewString(CsVM* vm, CsStr str);
CsValue csNewAstring(CsVM* vm, CsStr str);
CsValue csNewUstring(CsVM* vm, CsStr str, uint32_t charLen);

CsValue csNewTuple(CsVM* vm, const CsValue* vals, size_t len);
CsValue csNewEmptyList(CsVM* vm);
CsValue csNewList(CsVM* vm, const CsValue* vals, size_t len);
CsValue csNewEmptyMap(CsVM* vm);
CsValue csNewUntypedFunc(CsVM* vm, uint32_t numParams, CsFuncFn func);
CsValue csNewFunc(CsVM* vm, const CsTypeId* params, uint32_t numParams, CsTypeId retType, CsFuncFn func);
CsValue csNewPointer(CsVM* vm, void* ptr);
CsValue csNewType(CsVM* vm, CsTypeId type_id);

// Instantiating a `#host type` requires the `typeId` obtained from `CsTypeLoader` and
// the number of bytes the object will occupy. Objects of the same type can have different sizes.
// A `CsValue` which contains the object pointer is returned. Call `csAsHostObject` to obtain the pointer
// or use `csNewHostObjectPtr` to instantiate instead.
CsValue csNewHostObject(CsVM* vm, CsTypeId typeId, size_t n);

// Like `csNewHostObject` but returns the object's pointer. Wrap it into a value with `csHostObject`.
void* csNewHostObjectPtr(CsVM* vm, CsTypeId typeId, size_t n);

// Instantiates a standard object type.
CsValue csNewVmObject(CsVM* vm, CsTypeId typeId);

// Values.
CsTypeId csGetTypeId(CsValue val);
CsStr csNewValueDump(CsVM* vm, CsValue val);

// Values to C.
double csAsFloat(CsValue val);
bool csToBool(CsValue val);
bool csAsBool(CsValue val);
int64_t csAsInteger(CsValue val);
uint32_t csAsSymbolId(CsValue val);
CsStr csToTempString(CsVM* vm, CsValue val);
CsStr csToTempRawString(CsVM* vm, CsValue val);
void* csAsHostObject(CsValue val);

// Lists.
size_t csListLen(CsValue list);
size_t csListCap(CsValue list);
CsValue csListGet(CsVM* vm, CsValue list, size_t idx);
void csListSet(CsVM* vm, CsValue list, size_t idx, CsValue val);
void csListAppend(CsVM* vm, CsValue list, CsValue val);
void csListInsert(CsVM* vm, CsValue list, size_t idx, CsValue val);

// Maps.
// size_t csMapSize(CsValue map);
// bool csMapContains(CsValue map, CsValue key);
// bool csMapContainsStringKey(CsValue map, CsStr key);
// CsValue csMapGet(CsVM* vm, CsValue map, CsValue key);
// CsValue csMapGetStringKey(CsVM* vm, CsValue map, CsStr key);
// void csMapSet(CsVM* vm, CsValue map, CsValue key, CsValue val);
// void csMapSetStringKey(CsVM* vm, CsValue map, CsStr key, CsValue val);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif // !CYBER_H