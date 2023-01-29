#include <string.h>
#include "cyber.h"

// Compile this program with a C compiler. `zig cc` is used here as an example.
// zig cc main.c -I src -lcyber -L <Path to cyber.dll/so/dylib> -o main.exe

int main() {
    UserVM* vm = cyVmCreate();
    char* src = 
        "a = 2\n"
        "print a";
    uint64_t val = cyVmEval(vm, src, strlen(src));
    cyVmDestroy(vm);
    return 0;
}