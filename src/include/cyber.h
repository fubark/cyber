// Copyright (c) 2023 Cyber (See LICENSE)
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

typedef struct CsVM CsVM;

typedef uint64_t CsValue;

typedef struct CsModule CsModule;
typedef uint32_t CsModuleId;

#define CS_NULLID UINT32_MAX

typedef enum {
    CS_SUCCESS = 0,
    CS_ERROR_TOKEN,
    CS_ERROR_PARSE,
    CS_ERROR_COMPILE,
    CS_ERROR_PANIC,
    CS_ERROR_UNKNOWN,
} CsResultCode;

typedef enum {
    CS_TYPE_NONE = 0,
    CS_TYPE_BOOLEAN,
    CS_TYPE_ERROR,
    CS_TYPE_STATICASTRING,
    CS_TYPE_STATICUSTRING,
    CS_TYPE_ENUM,
    CS_TYPE_SYMBOL,
    CS_TYPE_INTEGER,
    CS_TYPE_FLOAT,
    CS_TYPE_LIST,
    CS_TYPE_LISTITER,
    CS_TYPE_MAP,
    CS_TYPE_MAPITER,
    CS_TYPE_CLOSURE,
    CS_TYPE_LAMBDA,
    CS_TYPE_ASTRING,
    CS_TYPE_USTRING,
    CS_TYPE_STRINGSLICE,
    CS_TYPE_RAWSTRING,
    CS_TYPE_RAWSTRINGSLICE,
    CS_TYPE_FIBER,
    CS_TYPE_BOX,
    CS_TYPE_NATIVEFUNC1,
    CS_TYPE_TCCSTATE,
    CS_TYPE_POINTER,
    CS_TYPE_FILE,
    CS_TYPE_DIR,
    CS_TYPE_DIRITER,
    CS_TYPE_METATYPE,
} CsType;
typedef uint32_t CsTypeId;

// Cyber deals with string slices internally for efficiency.
// Although some API functions could accept a null terminated string,
// it's more consistent to use CsStr everywhere.
// Creating a CsStr can be simplified with a macro:
// #define str(x) ((CsStr){ x, strlen(x) })
// Note: Returned `CsStr`s do not always end with a null char.
typedef struct CsStr {
    const char* buf;
    size_t len;
} CsStr;

// Top level.
CsStr csGetFullVersion();
CsStr csGetVersion();
CsStr csGetBuild();
CsStr csGetCommit();

// @host func is binded to this function pointer signature.
typedef CsValue (*CsHostFuncFn)(CsVM* vm, CsValue* args, uint8_t nargs);

// Given the current module's resolved URI and the "to be" imported module specifier,
// write the resolved specifier in `outUri` and return true, otherwise return false.
// Most embedders do not need a resolver and can rely on the default resolver which
// simply returns `spec` without any adjustments.
typedef bool (*CsModuleResolverFn)(CsVM* vm, uint32_t chunkId, CsStr curUri, CsStr spec, CsStr* outUri);

// Callback invoked after all symbols in module's src are loaded.
// This would be a convenient time to inject symbols not declared in the module's src.
typedef void (*CsPostLoadModuleFn)(CsVM* vm, uint32_t modId);

// Callback invoked just before the module is destroyed.
// When you have explicity injected symbols into a module from `CsPostLoadModuleFn`,
// this can be a convenient time to do cleanup logic (eg. release objects).
typedef void (*CsModuleDestroyFn)(CsVM* vm, uint32_t modId);

// Info about a @host func.
typedef struct CsHostFuncInfo {
    // The module it belongs to.
    uint32_t modId;
    // The name of the func.
    CsStr name;
    // The function's signature.
    uint32_t funcSigId;
    // A counter that tracks it's current position among all @host funcs in the module.
    // This is useful if you want to bind an array of function pointers to @host funcs.
    uint32_t idx;
} CsHostFuncInfo;

// Given info about a @host func, return it's function pointer or null.
typedef CsHostFuncFn (*CsHostFuncLoaderFn)(CsVM* vm, CsHostFuncInfo funcInfo);

// Module loader config.
typedef struct CsModuleLoaderResult {
    // The Cyber source code for the module.
    CsStr src;
    // Whether the provided `src` is from static memory or heap memory.
    bool srcIsStatic;
    // Pointer to callback or null.
    CsHostFuncLoaderFn funcLoader;
    // Pointer to callback or null.
    CsPostLoadModuleFn postLoad;
    // Pointer to callback or null.
    CsModuleDestroyFn destroy;
} CsModuleLoaderResult;

// Given the resolved import specifier of the module, write the loader details in `out`
// and return true, otherwise return false.
typedef bool (*CsModuleLoaderFn)(CsVM* vm, CsStr resolvedSpec, CsModuleLoaderResult* out);

// Override the behavior of `print` from the `builtins` module.
// The default behavior is a no-op.
typedef void (*CsPrintFn)(CsVM* vm, CsStr str);

// Stats of a GC run.
typedef struct CsGCResult {
    // Objects freed that were part of a reference cycle.
    uint32_t numCycFreed;
    // Total number of objects freed.
    uint32_t numObjFreed;
} CsGCResult;

//
// [ VM ]
//
CsVM* csCreate();
void csDestroy(CsVM* vm);
CsModuleResolverFn csGetModuleResolver(CsVM* vm);
void csSetModuleResolver(CsVM* vm, CsModuleResolverFn resolver);
CsModuleLoaderFn csGetModuleLoader(CsVM* vm);
void csSetModuleLoader(CsVM* vm, CsModuleLoaderFn loader);
CsPrintFn csGetPrint(CsVM* vm);
void csSetPrint(CsVM* vm, CsPrintFn print);
CsResultCode csEval(CsVM* vm, CsStr src, CsValue* outVal);
CsResultCode csValidate(CsVM* vm, CsStr src);
CsStr csGetLastErrorReport(CsVM* vm);
void* csGetUserData(CsVM* vm);
void csSetUserData(CsVM* vm, void* userData);

// Memory.
void csRelease(CsVM* vm, CsValue val);
void csRetain(CsVM* vm, CsValue val);
CsGCResult csPerformGC(CsVM* vm);

// Modules.
void csSetModuleFunc(CsVM* vm, CsModuleId modId, CsStr name, uint32_t numParams, CsHostFuncFn func);
void csSetModuleVar(CsVM* vm, CsModuleId modId, CsStr name, CsValue val);

// For embedded, Cyber by default uses malloc (it can be configured to use the high-perf mimalloc).
// If the host uses a different allocator than Cyber, use `csAlloc` to allocate memory
// that is handed over to Cyber so it knows how to free it.
// This is also used to manage accessible buffers when embedding WASM.
void* csAlloc(CsVM* vm, size_t size);
void csFree(CsVM* vm, void* ptr, size_t len);

//
// [ Values ]
//

// Create values.
CsValue csNone();
CsValue csTrue();
CsValue csFalse();
CsValue csFloat(double n);
CsValue csInteger(int n);
CsValue csTagLiteral(CsVM* vm, CsStr str);
CsValue csNewString(CsVM* vm, CsStr str);
CsValue csNewAstring(CsVM* vm, CsStr str);
CsValue csNewUstring(CsVM* vm, CsStr str, uint32_t charLen);
CsValue csNewList(CsVM* vm);
CsValue csNewMap(CsVM* vm);
CsValue csNewHostFunc(CsVM* vm, CsHostFuncFn func, uint32_t numParams);
CsValue csNewPointer(CsVM* vm, void* ptr);

// Values.
CsTypeId csGetTypeId(CsValue val);

// Values to C.
double csAsFloat(CsValue val);
bool csToBool(CsValue val);
bool csAsBool(CsValue val);
int64_t csAsInteger(CsValue val);
uint32_t csAsTagLiteralId(CsValue val);
CsStr csToTempString(CsVM* vm, CsValue val);
CsStr csToTempRawString(CsVM* vm, CsValue val);

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