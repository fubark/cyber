#include <string.h>
#include <stdio.h>
#include "cyber.h"

// Compile this program with a C compiler. `zig cc` is used here as an example.
// zig cc main.c -I src -lcyber -L <Path to cyber.dll/so/dylib> -o main.exe

void loadCore(CyUserVM* vm, CyModule* mod);
CyValue add(CyUserVM* vm, CyValue* args, uint8_t nargs);

int main() {
    CyUserVM* vm = cyVmCreate();
    cyVmAddModuleLoader(vm, cstr("core"), loadCore);

    CStr src = cstr(
        "a = 2\n"
        "print add(a, 100)"
    );
    CyValue val;
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

void loadCore(CyUserVM* vm, CyModule* mod) {
    cyVmSetModuleFunc(vm, mod, cstr("add"), 2, add);
}

CyValue add(CyUserVM* vm, CyValue* args, uint8_t nargs) {
    double left = cyValueAsDouble(args[0]);
    double right = cyValueAsDouble(args[1]);
    return cyValueNumber(left + right);
}