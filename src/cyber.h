// Copyright (c) 2023 Cyber (See LICENSE)
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct CyVM CyVM;
typedef struct CyModule CyModule;
typedef uint64_t CyValue;

#define CY_NullId UINT32_MAX

typedef enum {
    CY_Success = 0,
    CY_ErrorToken,
    CY_ErrorParse,
    CY_ErrorCompile,
    CY_ErrorPanic,
    CY_ErrorUnknown,
} CyResultCode;

typedef enum {
    CY_TypeNone = 0,
    CY_TypeBoolean,
    CY_TypeError,
    CY_TypeStaticAstring,
    CY_TypeStaticUstring,
    CY_TypeUserTag,
    CY_TypeUserTagLiteral,
    CY_TypeInteger,
    CY_TypeNumber,
    CY_TypeList,
    CY_TypeListIter,
    CY_TypeMap,
    CY_TypeMapIter,
    CY_TypeClosure,
    CY_TypeLambda,
    CY_TypeAstring,
    CY_TypeUstring,
    CY_TypeStringSlice,
    CY_TypeRawString,
    CY_TypeRawStringSlice,
    CY_TypeFiber,
    CY_TypeBox,
    CY_TypeNativeFunc1,
    CY_TypeTccState,
    CY_TypePointer,
    CY_TypeFile,
    CY_TypeDir,
    CY_TypeDirIter,
    CY_TypeSymbol,
} CyType;

// Null terminated string, but also includes a length.
typedef struct CStr {
    const char* charz;
    size_t len;
} CStr;
#define cstr(X) (CStr){ X, strlen(X) }

typedef uint32_t CyTypeId;

typedef CyValue (*CyFunc)(CyVM* vm, CyValue* args, uint8_t nargs);
typedef bool (*CyLoadModuleFunc)(CyVM* vm, CyModule* mod);

// Top level.
CStr cyGetFullVersion();
CStr cyGetVersion();
CStr cyGetBuild();
CStr cyGetCommit();

// VM.
CyVM* cyVmCreate();
void cyVmDestroy(CyVM* vm);
CyResultCode cyVmEval(CyVM* vm, CStr src, CyValue* outVal);
CyResultCode cyVmValidate(CyVM* vm, CStr src);
CStr cyVmGetLastErrorReport(CyVM* vm);
void cyVmRelease(CyVM* vm, CyValue val);
void cyVmRetain(CyVM* vm, CyValue val);
void* cyVmGetUserData(CyVM* vm);
void cyVmSetUserData(CyVM* vm, void* userData);

// Modules.
void cyVmAddModuleLoader(CyVM* vm, CStr name, CyLoadModuleFunc func);
void cyVmSetModuleFunc(CyVM* vm, CyModule* mod, CStr name, uint32_t numParams, CyFunc func);
void cyVmSetModuleVar(CyVM* vm, CyModule* mod, CStr name, CyValue val);

// Intended to be used to manage accessible buffers when embedding WASM.
void* cyVmAlloc(CyVM* vm, size_t size);
void cyVmFree(CyVM* vm, void* ptr, size_t len);

// Initialize values.
CyValue cyValueNone();
CyValue cyValueTrue();
CyValue cyValueFalse();
CyValue cyValueNumber(double n);
CyValue cyValueInteger(int n);
CyValue cyValueGetOrAllocStringInfer(CyVM* vm, CStr str);
CyValue cyValueGetOrAllocAstring(CyVM* vm, CStr str);
CyValue cyValueGetOrAllocUstring(CyVM* vm, CStr str, uint32_t charLen);
CyValue cyValueAllocList(CyVM* vm);
CyValue cyValueAllocMap(CyVM* vm);
CyValue cyValueAllocNativeFunc(CyVM* vm, CyFunc func, uint32_t numParams);
CyValue cyValueAllocPointer(CyVM* vm, void* ptr);
CyValue cyValueTagLiteral(CyVM* vm, CStr str);

// Values.
CyTypeId cyValueGetTypeId(CyValue val);

// Values to C types.
double cyValueAsNumber(CyValue val);
bool cyValueToBool(CyValue val);
bool cyValueAsBool(CyValue val);
int cyValueAsInteger(CyValue val);
uint32_t cyValueAsTagLiteralId(CyValue val);
CStr cyValueToTempString(CyVM* vm, CyValue val);
CStr cyValueToTempRawString(CyVM* vm, CyValue val);

// Lists.
size_t cyListLen(CyValue list);
size_t cyListCap(CyValue list);
CyValue cyListGet(CyVM* vm, CyValue list, size_t idx);
void cyListSet(CyVM* vm, CyValue list, size_t idx, CyValue val);
void cyListAppend(CyVM* vm, CyValue list, CyValue val);
void cyListInsert(CyVM* vm, CyValue list, size_t idx, CyValue val);

// Maps.
// size_t cyMapSize(CyValue map);
// bool cyMapContains(CyValue map, CyValue key);
// bool cyMapContainsStringKey(CyValue map, CStr key);
// CyValue cyMapGet(CyVM* vm, CyValue map, CyValue key);
// CyValue cyMapGetStringKey(CyVM* vm, CyValue map, CStr key);
// void cyMapSet(CyVM* vm, CyValue map, CyValue key, CyValue val);
// void cyMapSetStringKey(CyVM* vm, CyValue map, CStr key, CyValue val);