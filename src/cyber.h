// Copyright (c) 2023 Cyber (See LICENSE)
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct CyUserVM CyUserVM;
typedef struct CyModule CyModule;
typedef uint64_t CyValue;

const uint32_t CY_NullId = UINT32_MAX;

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
    CY_TypeOpaquePtr,
    CY_TypeFile,
    CY_TypeDir,
    CY_TypeDirIter,
    CY_TypeSymbol,
};

// Null terminated string, but also includes a length.
typedef struct CStr {
    char* charz;
    size_t len;
} CStr;
#define cstr(X) (CStr){ X, strlen(X) }

typedef uint32_t CyTypeId;

typedef CyValue (*CyFunc)(CyUserVM* vm, CyValue* args, uint8_t nargs);
typedef bool (*CyLoadModuleFunc)(CyUserVM* vm, CyModule* mod);

// VM.
CyUserVM* cyVmCreate();
void cyVmDestroy(CyUserVM* vm);
CyResultCode cyVmEval(CyUserVM* vm, CStr src, CyValue* outVal);
CStr cyVmGetLastErrorReport(CyUserVM* vm);
void cyVmRelease(CyUserVM* vm, CyValue val);
void cyVmRetain(CyUserVM* vm, CyValue val);
void* cyVmGetUserData(CyUserVM* vm);
void cyVmSetUserData(CyUserVM* vm, void* userData);

// Modules.
void cyVmAddModuleLoader(CyUserVM* vm, CStr name, CyLoadModuleFunc func);
void cyVmSetModuleFunc(CyUserVM* vm, CyModule* mod, CStr name, uint32_t numParams, CyFunc func);
void cyVmSetModuleVar(CyUserVM* vm, CyModule* mod, CStr name, CyValue val);

// Intended to be used to manage accessible buffers when embedding WASM.
void* cyVmAlloc(CyUserVM* vm, size_t size);
void cyVmFree(CyUserVM* vm, void* ptr, size_t len);

// Initialize values.
CyValue cyValueNone();
CyValue cyValueTrue();
CyValue cyValueFalse();
CyValue cyValueNumber(double n);
CyValue cyValueInteger(int n);
CyValue cyValueGetOrAllocStringInfer(CyUserVM* vm, CStr str);
CyValue cyValueGetOrAllocAstring(CyUserVM* vm, CStr str);
CyValue cyValueGetOrAllocUstring(CyUserVM* vm, CStr str, uint32_t charLen);
CyValue cyValueAllocList(CyUserVM* vm);
CyValue cyValueAllocMap(CyUserVM* vm);

// Values.
CyTypeId cyValueGetTypeId(CyValue val);

// Values to C types.
double cyValueAsNumber(CyValue val);
bool cyValueToBool(CyValue val);
bool cyValueAsBool(CyValue val);
int cyValueAsInteger(CyValue val);
uint32_t asTagLiteralId(CyValue val);
CStr cyValueToTempString(CyUserVM* vm, CyValue val);
CStr cyValueToTempRawString(CyUserVM* vm, CyValue val);