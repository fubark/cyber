// Copyright (c) 2023 Cyber (See LICENSE)

#include <stdint.h>

typedef struct UserVM UserVM;

UserVM* cyVmCreate();
void cyVmDestroy(UserVM* vm);
uint64_t cyVmEval(UserVM* vm, const char* src, uintptr_t size);