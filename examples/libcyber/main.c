#include <stdio.h>
#include <string.h>
#include "cyber.h"

// Build `libcyber.a` with Zig:
// zig build lib

// Compile this program with any C compiler:
// clang -o main main.c -I ../../src/include ../../zig-out/lib/libcyber.a

void printer(CLThread* t, CLBytes str) {
    if (str.len == 1 && str.ptr[0] == '\n') {
        // Skip second invocation by `print` for the new line character.
        return;
    }
    printf("My print: %.*s\n", (int)str.len, str.ptr);
}

int main() {
    CLVM* vm = cl_vm_init();
    cl_vm_set_printer(vm, printer);

    const char* src =
        "a := 1\n"
        "print(a + 2)\n";

    CLEvalResult res;
    CLResultCode code = cl_vm_eval(vm, CL_BYTES(src), &res);
    if (code == CL_SUCCESS) {
        printf("Success!\n");
    } else { 
        CLBytes summary = cl_vm_error_summary(vm);
        printf("%.*s\n", (int)summary.len, summary.ptr);
        cl_vm_freeb(vm, summary);
    }
    cl_vm_deinit(vm);
    return 0;
}