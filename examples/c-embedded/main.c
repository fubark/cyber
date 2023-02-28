#include <stdio.h>
#include "cyber.h"

// Compile this program with a C compiler. `zig cc` is used here as an example.
// zig cc main.c -I src -lcyber -L <Path to cyber.dll/so/dylib> -o main.exe

int main() {
    UserVM* vm = cyVmCreate();
    CStr src = cstr(
        "a = 2\n"
        "print a"
    );
    Value val;
    int res = cyVmEval(vm, src, &val);
    if (res == CY_Success) {
        printf("Success!\n");
        cyVmRelease(vm, val);
    } else {
        CStr err = cyVmGetLastErrorReport(vm);
        printf("%s\n", err.charz);
    }
    cyVmDestroy(vm);
    return 0;
}