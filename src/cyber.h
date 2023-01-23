// Copyright (c) 2023 Cyber (See LICENSE)

#include <stdint.h>

#define bool _Bool

typedef struct UserVM UserVM;
typedef uint64_t Value;

UserVM* cyVmCreate();
void cyVmDestroy(UserVM* vm);
uint64_t cyVmEval(UserVM* vm, const char* src, uintptr_t size);

bool cyValueIsPanic(Value val);