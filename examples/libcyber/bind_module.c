#include <string.h>
#include <stdio.h>
#include "cyber.h"

// This example shows how to setup a module loader for custom modules.
// Declarations from a module's source code invokes callbacks to bind with the host's functions and types.
// To see how to inject symbols programmatically, see `inject_module.c`.

// Build `libcyber.a` with Zig:
// zig build lib

// Compile this program with a C compiler. `zig cc` is used here as an example.
// clang -o bind_module bind_module.c -I ../../src/include ../../zig-out/lib/libcyber.a

// ----------------

// Types in Cyber mimic C's type layout.
typedef struct MyNode {
    double data;
} MyNode;

CLRet add(CLThread* t) {
    double* ret = cl_thread_ret(t, sizeof(double));
    double a = cl_thread_float(t);
    double b = cl_thread_float(t);
    *ret = a * b;
    return CL_RET_OK;
}

CLRet mynode_init(CLThread* t) {
    MyNode* ret = cl_thread_ret(t, sizeof(MyNode));
    *ret = (MyNode){ .data = 123.0 };
    return CL_RET_OK;
}

CLRet mynode_deinit(CLThread* t) {
    cl_thread_ret(t, 0);
    MyNode* self = (MyNode*)cl_thread_ptr(t);
    printf("MyNode deinit.\n");
    return CL_RET_OK;
}

CLRet mynode_compute(CLThread* t) {
    double* ret = cl_thread_ret(t, sizeof(double));
    MyNode* self = (MyNode*)cl_thread_ptr(t);
    *ret = self->data * 100;
    return CL_RET_OK;
}

CLValue load_myglobal(CLVM* vm) {
    return CL_BITCAST(CLValue, 234.0);
}

// This module loader provides the source code and callbacks to load @host funcs, vars, and types.
bool loader(CLVM* vm, CLSym* mod, CLBytes uri, CLBytes* out_src) {
    if (strncmp("my_mod", uri.ptr, uri.len) == 0) {
        const char* src = (
            "#[bind] fn add(a, b float) -> float\n"
            "#[bind] global my_global float\n"
            "\n"
            "type MyNode:\n"
            "  data float\n"
            "\n"
            "#[bind] fn MyNode :: @init() -> MyNode\n"
            "#[bind] fn (&MyNode) @deinit()\n"
            "#[bind] fn (&MyNode) compute() -> float\n"
        );

        cl_mod_add_func(mod, CL_BYTES("add"), CL_BIND_FUNC(add));
        cl_mod_add_func(mod, CL_BYTES("MyNode.@init"), CL_BIND_FUNC(mynode_init));
        cl_mod_add_func(mod, CL_BYTES("MyNode.@deinit"), CL_BIND_FUNC(mynode_deinit));
        cl_mod_add_func(mod, CL_BYTES("MyNode.compute"), CL_BIND_FUNC(mynode_compute));
        cl_mod_add_global(mod, CL_BYTES("my_global"), load_myglobal);

        *out_src = cl_new_bytes(vm, strlen(src));
        memcpy((void*)out_src->ptr, src, strlen(src));
        return true;
    } else {
        // Fallback to the default module loader to load builtin modules such as `core`.
        return cl_default_loader(vm, mod, uri, out_src);
    }
}

void printer(CLThread* t, CLBytes str) {
    if (str.len == 1 && str.ptr[0] == '\n') {
        // Skip second invocation by `print` for the new line character.
        return;
    }
    printf("My print: %.*s\n", (int)str.len, str.ptr);
}

int main() {
    // clVerbose = true;
    CLVM* vm = cl_vm_init();
    cl_vm_set_loader(vm, loader);
    cl_vm_set_printer(vm, printer);

    const char* main =
        "use my_mod\n"
        "\n"
        "a := 1.0\n"
        "print(my_mod.add(a, 2))\n"
        "print(my_mod.my_global)\n"
        "\n"
        "-- init MyNode\n"
        "n := my_mod.MyNode()\n"
        "print(n.compute())\n";

    CLEvalResult eval;
    CLResultCode res = cl_vm_eval(vm, CL_BYTES(main), &eval);
    if (res == CL_SUCCESS) {
        printf("Success!\n");
    } else {
        CLBytes summary = cl_vm_error_summary(vm);
        printf("%.*s\n", (int)summary.len, summary.ptr);
        cl_vm_free(vm, summary);
    }

    // Check that all references were accounted for. (Should be 0)
    CLThread* t = cl_vm_main_thread(vm);
    printf("Live objects: %zu\n", cl_thread_count_objects(t));
    cl_vm_deinit(vm);
    return 0;
}