// Copyright (c) 2023 Cyber (See LICENSE)
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

typedef struct CsVM CsVM;

typedef uint64_t CsValue;
typedef struct CsValueSlice {
    CsValue* ptr;
    size_t len;
} CsValueSlice;

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
    CS_TYPE_TUPLE,
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
    CS_TYPE_METATYPE,
} CsType;
typedef uint32_t CsTypeId;
typedef uint32_t CsSemaTypeId;

// Cyber deals with string slices internally for efficiency.
// Although some API functions could accept a null terminated string,
// it's more consistent to use CsStr everywhere.
// Creating a CsStr can be simplified with a macro:
// #define str(x) ((CsStr){ x, strlen(x) })
// NOTE: Returned `CsStr`s do not always end with a null char.
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
typedef CsValue (*CsFuncFn)(CsVM* vm, const CsValue* args, uint8_t nargs);

// Internal @host func used to do inline caching.
typedef void (*CsQuickenFuncFn)(CsVM* vm, uint8_t* pc, const CsValue* args, uint8_t nargs);

// Given the current module's resolved URI and the "to be" imported module specifier,
// write the resolved specifier in `outUri` and return true, or return false.
// Most embedders do not need a resolver and can rely on the default resolver which
// simply returns `spec` without any adjustments.
typedef bool (*CsModuleResolverFn)(CsVM* vm, uint32_t chunkId, CsStr curUri, CsStr spec, CsStr* outUri);

// Callback invoked after all type symbols in the module's src are loaded.
// This could be used to set up an array or hashmap for binding @host vars.
typedef void (*CsPostTypeLoadModuleFn)(CsVM* vm, uint32_t modId);

// Callback invoked after all symbols in the module's src are loaded.
// This could be used to inject symbols not declared in the module's src.
typedef void (*CsPostLoadModuleFn)(CsVM* vm, uint32_t modId);

// Callback invoked just before the module is destroyed.
// This could be used to cleanup (eg. release) injected symbols from `CsPostLoadModuleFn`,
typedef void (*CsModuleDestroyFn)(CsVM* vm, uint32_t modId);

// Info about a @host func.
typedef struct CsFuncInfo {
    // The module it belongs to.
    uint32_t modId;
    // The name of the func.
    CsStr name;
    // The function's signature.
    uint32_t funcSigId;
    // A counter that tracks it's current position among all @host funcs in the module.
    // This is useful if you want to bind an array of function pointers to @host funcs.
    uint32_t idx;
} CsFuncInfo;

typedef enum {
    // Most @host funcs have this type.
    CS_FUNC_STANDARD,
    // Some internal functions need this to perform inline caching.
    CS_FUNC_QUICKEN,
} CsFuncType;

// Result given to Cyber when binding a @host func.
typedef struct CsFuncResult {
    // Pointer to the binded function. (CsFuncFn/CsQuickenFuncFn)
    void* ptr;
    // `CsFuncType`. By default, this is `CS_FUNC_STANDARD`.
    uint8_t type;
} CsFuncResult;

// Given info about a @host func, write it's function pointer to `out->ptr` and return true,
// or return false.
typedef bool (*CsFuncLoaderFn)(CsVM* vm, CsFuncInfo funcInfo, CsFuncResult* out);

// Info about a @host var.
typedef struct CsVarInfo {
    // The module it belongs to.
    uint32_t modId;
    // The name of the var.
    CsStr name;
    // A counter that tracks it's current position among all @host vars in the module.
    // This is useful if you want to bind an array of `CsValue`s to @host vars.
    uint32_t idx;
} CsVarInfo;

// Given info about a @host var, write a value to `out` and return true, or return false.
// The value is consumed by the module. If the value should outlive the module,
// call `csRetain` before handing it over.
typedef bool (*CsVarLoaderFn)(CsVM* vm, CsVarInfo funcInfo, CsValue* out);

// Info about a @host type.
typedef struct CsTypeInfo {
    // The module it belongs to.
    uint32_t modId;
    // The name of the type.
    CsStr name;
    // A counter that tracks it's current position among all @host types in the module.
    // This is useful if you want to bind an array of data to @host types.
    uint32_t idx;
} CsTypeInfo;

typedef enum {
    // @host object type that needs to be created.
    CS_TYPE_OBJECT,
    // @host object type that is hardcoded into the VM and already has a semantic and runtime type id.
    CS_TYPE_CORE_OBJECT,
} CsTypeType;

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

// Result given to Cyber when binding a @host type.
typedef struct CsTypeResult {
    union {
        struct {
            // The created runtime type id will be written to `outTypeId`.
            // This typeId is then used to allocate a new instance of the object.
            CsTypeId* outTypeId;
            // The created semantic type id will be written to `outSemaTypeId`.
            CsSemaTypeId* outSemaTypeId;
            // Pointer to callback or null.
            CsObjectGetChildrenFn getChildren;
            // Pointer to callback or null.
            CsObjectFinalizerFn finalizer;
        } object;
        struct {
            // Existing runtime typeId.
            CsTypeId typeId;
            // Existing semantic typeId.
            CsSemaTypeId semaTypeId;
        } coreObject;
    } data;
    // `CsTypeType`. By default, this is `CS_TYPE_OBJECT`.
    uint8_t type;
} CsTypeResult;

// Given info about a @host type, write the result to `out` and return true, or return false.
typedef bool (*CsTypeLoaderFn)(CsVM* vm, CsTypeInfo typeInfo, CsTypeResult* out);

// Module loader config.
typedef struct CsModuleLoaderResult {
    // The Cyber source code for the module.
    CsStr src;
    // Whether the provided `src` is from static memory or heap memory.
    bool srcIsStatic;
    // Pointer to callback or null.
    CsFuncLoaderFn funcLoader;
    // Pointer to callback or null.
    CsVarLoaderFn varLoader;
    // Pointer to callback or null.
    CsTypeLoaderFn typeLoader;
    // Pointer to callback or null.
    CsPostTypeLoadModuleFn preLoad;
    // Pointer to callback or null.
    CsPostLoadModuleFn postLoad;
    // Pointer to callback or null.
    CsModuleDestroyFn destroy;
} CsModuleLoaderResult;

// Given the resolved import specifier of the module, write the loader details in `out`
// and return true, or return false.
typedef bool (*CsModuleLoaderFn)(CsVM* vm, CsStr resolvedSpec, CsModuleLoaderResult* out);

// Override the behavior of `print` from the `builtins` module.
// The default behavior is a no-op.
typedef void (*CsPrintFn)(CsVM* vm, CsStr str);

//
// [ VM ]
//

CsVM* csCreate();

// Deinitialize the VM but leaves heap pages alive to allow object counting.
// Afterwards, call `csDestroy` or perform a check on `csCountObjects`.
void csDeinit(CsVM* vm);

// Deinitializes the VM and frees all memory associated with it. Any operation on `vm` afterwards is undefined.
void csDestroy(CsVM* vm);

CsModuleResolverFn csGetModuleResolver(CsVM* vm);
void csSetModuleResolver(CsVM* vm, CsModuleResolverFn resolver);

// The default module resolver. It returns `spec`.
bool csDefaultModuleResolver(CsVM* vm, uint32_t chunkId, CsStr curUri, CsStr spec, CsStr* outUri);

CsModuleLoaderFn csGetModuleLoader(CsVM* vm);
void csSetModuleLoader(CsVM* vm, CsModuleLoaderFn loader);

// The default module loader. It knows how to load the `builtins` module.
bool csDefaultModuleLoader(CsVM* vm, CsStr resolvedSpec, CsModuleLoaderResult* out);

CsPrintFn csGetPrint(CsVM* vm);
void csSetPrint(CsVM* vm, CsPrintFn print);

// Evalutes the source code and returns the result code.
// If the last statement of the script is an expression, `outVal` will contain the value.
CsResultCode csEval(CsVM* vm, CsStr src, CsValue* outVal);
CsResultCode csValidate(CsVM* vm, CsStr src);

/// After receiving an error CsResultCode, this returns the error report. Call `csFreeStr` afterwards.
CsStr csAllocLastErrorReport(CsVM* vm);

// Attach a userdata pointer inside the VM.
void* csGetUserData(CsVM* vm);
void csSetUserData(CsVM* vm, void* userData);

// Verbose flag. In a debug build, this would print more logs.
extern bool csVerbose;

// Modules.
void csSetModuleFunc(CsVM* vm, CsModuleId modId, CsStr name, uint32_t numParams, CsFuncFn func);
void csSetModuleVar(CsVM* vm, CsModuleId modId, CsStr name, CsValue val);

// 
// [ Memory ]
//
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

// For embedded, Cyber by default uses malloc (it can be configured to use the high-perf mimalloc).
// If the host uses a different allocator than Cyber, use `csAlloc` to allocate memory
// that is handed over to Cyber so it knows how to free it.
// This is also used to manage accessible buffers when embedding WASM.
void* csAlloc(CsVM* vm, size_t size);
void csFree(CsVM* vm, void* ptr, size_t len);
void csFreeStr(CsVM* vm, CsStr str);

//
// [ Values ]
// Functions that begin with `csNew` instantiate objects on the heap and
// automatically retain +1 refcount.
//

// Create values.
CsValue csNone();
CsValue csTrue();
CsValue csFalse();
CsValue csBool(bool b);

// int64_t is downcasted to a 48-bit int.
CsValue csInteger(int64_t n);
CsValue csInteger32(int32_t n);
CsValue csFloat(double f);
CsValue csHostObject(void* ptr);
CsValue csSymbol(CsVM* vm, CsStr str);

// `csNewString` is the recommended way to create a new string. Use `Astring` or `Ustring` if you know it
// will be ASCII or UTF-8 respectively.
CsValue csNewString(CsVM* vm, CsStr str);
CsValue csNewAstring(CsVM* vm, CsStr str);
CsValue csNewUstring(CsVM* vm, CsStr str, uint32_t charLen);

CsValue csNewEmptyList(CsVM* vm);
CsValue csNewList(CsVM* vm, const CsValue* vals, size_t len);
CsValue csNewEmptyMap(CsVM* vm);
CsValue csNewFunc(CsVM* vm, CsFuncFn func, uint32_t numParams);
CsValue csNewPointer(CsVM* vm, void* ptr);

// Instantiating a `@host type object` requires the `typeId` obtained from `CsTypeLoader` and
// the number of bytes the object will occupy. Objects of the same type can have different sizes.
// A `CsValue` which contains the object pointer is returned. Call `csAsHostObject` to obtain the pointer
// or use `csNewHostObjectPtr` to instantiate instead.
CsValue csNewHostObject(CsVM* vm, CsTypeId typeId, size_t n);

// Like `csNewHostObject` but returns the object's pointer. Wrap it into a value with `csHostObject`.
void* csNewHostObjectPtr(CsVM* vm, CsTypeId typeId, size_t n);

// Instantiates a standard `type object`.
CsValue csNewVmObject(CsVM* vm, CsTypeId typeId);

// Values.
CsTypeId csGetTypeId(CsValue val);

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