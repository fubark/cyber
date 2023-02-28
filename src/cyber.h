// Copyright (c) 2023 Cyber (See LICENSE)

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

UserVM* cyVmCreate();
void cyVmDestroy(UserVM* vm);
ResultCode cyVmEval(UserVM* vm, const char* src, uintptr_t srcLen, Value* outVal);
char* cyVmGetLastErrorReport(UserVM* vm);
void cyVmRelease(UserVM* vm, Value val);