#include <stdio.h>
#include <string.h>
#include "cyber.h"

// Compile this program with a C compiler. `zig cc` is used here as an example.
// zig cc main.c -I ../../src/include ../../zig-out/lib/libcyber.a -o main

#define STR(s) ((CLStr){ s, strlen(s) })

void printer(CLVM* vm, CLStr str) {
    if (str.len == 1 && str.ptr[0] == '\n') {
        // Skip second invocation by `print` for the new line character.
        return;
    }
    printf("My print: %.*s\n", (int)str.len, str.ptr);
}

int main() {
    CLVM* vm = clCreate();
    clSetPrinter(vm, printer);

    CLStr src = STR(
        "var a = 1\n"
        "print(a + 2)\n"
    );
    CLValue val;
    int res = clEval(vm, src, &val);
    if (res == CL_SUCCESS) {
        printf("Success!\n");
        clRelease(vm, val);
    } else { 
        CLStr s = clNewLastErrorSummary(vm);
        printf("%.*s\n", (int)s.len, s.ptr);
        clFree(vm, s);
    }
    clDestroy(vm);
    return 0;
}