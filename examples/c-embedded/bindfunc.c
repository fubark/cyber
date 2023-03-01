#include <string.h>
#include <stdio.h>
#include "cyber.h"

// Compile this program with a C compiler. `zig cc` is used here as an example.
// zig cc main.c -I src -lcyber -L <Path to cyber.dll/so/dylib> -o main.exe

bool loadCore(CyUserVM* vm, CyModule* mod);
bool loadMyMod(CyUserVM* vm, CyModule* mod);
CyValue add(CyUserVM* vm, CyValue* args, uint8_t nargs);

int main() {
    CyUserVM* vm = cyVmCreate();

    // Add "add" to the default module (Loaded into each script's namespace).
    cyVmAddModuleLoader(vm, cstr("core"), loadCore);

    // Add "add" to a custom builtin named "mymod".
    cyVmAddModuleLoader(vm, cstr("mymod"), loadMyMod);

    CStr src = cstr(
        "import mod 'mymod'\n"
        "\n"
        "a = 2\n"
        "print add(a, 100)\n"
        "print mod.add(a, a)"
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

bool loadMyMod(CyUserVM* vm, CyModule* mod) {
    cyVmSetModuleFunc(vm, mod, cstr("add"), 2, add);
    return true;
}

bool loadCore(CyUserVM* vm, CyModule* mod) {
    cyVmSetModuleFunc(vm, mod, cstr("add"), 2, add);
    return true;
}

CyValue add(CyUserVM* vm, CyValue* args, uint8_t nargs) {
    double left = cyValueAsDouble(args[0]);
    double right = cyValueAsDouble(args[1]);
    return cyValueNumber(left + right);
}