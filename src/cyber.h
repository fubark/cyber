// Copyright (c) 2023 Cyber (See LICENSE)
#include <string.h>
#include <stdint.h>

#define bool _Bool

typedef struct UserVM UserVM;
typedef uint64_t Value;

typedef enum {
    CY_Success = 0,
    CY_ErrorToken,
    CY_ErrorParse,
    CY_ErrorCompile,
    CY_ErrorPanic,
    CY_ErrorUnknown,
} ResultCode;

// Null terminated string, but also includes a length.
typedef struct CStr {
    char* charz;
    size_t len;
} CStr;
#define cstr(X) (CStr){ X, strlen(X) }

UserVM* cyVmCreate();
void cyVmDestroy(UserVM* vm);
ResultCode cyVmEval(UserVM* vm, CStr src, Value* outVal);
CStr cyVmGetLastErrorReport(UserVM* vm);
ModuleId cyVmGetModule(UserVM* vm, CStr name);
void cyVmRelease(UserVM* vm, Value val);

// Intended to be used to manage accessible buffers when embedding WASM.
void* cyVmAlloc(UserVM* vm, size_t size);
void cyVmFree(UserVM* vm, void* ptr, size_t len);