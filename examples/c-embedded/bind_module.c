#include <string.h>
#include <stdio.h>
#include "cyber.h"

// This example shows how to setup a module loader for custom modules.
// Declarations from a module's source code invokes callbacks to bind with the host's functions and types.
// To see how to inject symbols programmatically, see `inject_module.c`.

// Compile this program with a C compiler. `zig cc` is used here as an example.
// zig cc bind_module.c -I ../../src/include ../../zig-out/lib/libcyber.a -o main

// Convenience macros to deal with Cyber string slices.
#define STR(s) ((CLStr){ s, strlen(s) })

CLValue add(CLVM* vm, const CLValue* args, uint8_t nargs) {
    double res = clAsFloat(args[0]) + clAsFloat(args[1]);
    return clFloat(res);
}

// Forward declaration.
CLValue myNodeNew(CLVM* vm, const CLValue* args, uint8_t nargs);
CLValue myNodeAsList(CLVM* vm, const CLValue* args, uint8_t nargs);

CLHostFuncEntry funcs[] = {
    CL_FUNC("add",           add),
    CL_FUNC("MyNode.asList", myNodeAsList),
    CL_FUNC("MyNode.new",    myNodeNew),
};

// C has limited static initializers (and objects need a vm instance) so initialize them in `main`.
typedef struct { char* n; CLValue v; } NameValue;
NameValue vars[2];

bool varLoader(CLVM* vm, CLVarInfo info, CLValue* out) {
    // Check that the name matches before setting the value.
    if (strncmp(vars[info.idx].n, info.name.ptr, info.name.len) == 0) {
        // Objects are consumed by the module.
        *out = vars[info.idx].v;
        return true;
    } else {
        return false;
    }
}

// Binding a C struct with it's own children and finalizer.
// This struct retains 2 VM values and has 2 arbitrary data values unrelated to the VM.
typedef struct MyNode {
    CLValue val1;
    CLValue val2;
    int a;
    double b;
} MyNode;

CLTypeId myNodeId;

// Implement the `new` function in MyNode.
CLValue myNodeNew(CLVM* vm, const CLValue* args, uint8_t nargs) {
    // Instantiate our object.
    CLValue new = clNewHostObject(vm, myNodeId, sizeof(MyNode));
    MyNode* my = (MyNode*)clAsHostObject(new);

    // Assign the constructor args passed in and retain them since the new object now references them.
    clRetain(vm, args[0]);
    my->val1 = args[0];
    clRetain(vm, args[1]);
    my->val2 = args[1];

    // Assign non VM values.
    my->a = 123;
    my->b = 9999.999;
    return new;
}

// Implement the `asList` method in MyNode.
CLValue myNodeAsList(CLVM* vm, const CLValue* args, uint8_t nargs) {
    // First argument is `self`.
    MyNode* my = (MyNode*)clAsHostObject(args[0]);

    CLValue vals[4] = {my->val1, my->val2, clInteger(my->a), clFloat(my->b)};
    return clNewListDyn(vm, &vals[0], 4);
}

CLValueSlice myNodeGetChildren(CLVM* vm, void* obj) {
    MyNode* my = (MyNode*)obj;
    return (CLValueSlice){ .ptr = &my->val1, .len = 2 };
}

void myNodeFinalizer(CLVM* vm, void* obj) {
    printf("MyNode finalizer was called.\n");
}

CLHostTypeEntry types[] = {
    CL_CUSTOM_TYPE("MyNode", &myNodeId, myNodeGetChildren, myNodeFinalizer),
};

// This module loader provides the source code and callbacks to load @host funcs, vars, and types.
bool modLoader(CLVM* vm, CLStr spec, CLModule* res) {
    if (strncmp("my_mod", spec.ptr, spec.len) == 0) {
        CLStr src = STR(
            "@host func add(a float, b float) float\n"
            "@host var .MyConstant float\n"
            "@host var .MyList     List[dyn]\n"
            "\n"
            "@host\n"
            "type MyNode _:\n"
            "    @host func asList() any"
            "\n"
            "@host func MyNode.new(a any, b any) MyNode\n"
        );
        *res = clCreateModule(vm, spec, src);
        CLModuleConfig config = (CLModuleConfig){
            .funcs = (CLSlice){ .ptr = funcs, .len = 3 },
            .types = (CLSlice){ .ptr = types, .len = 1 },
            .varLoader = varLoader,
        };
        clSetModuleConfig(vm, *res, &config);
        return true;
    } else {
        // Fallback to the default module loader to load `builtins`.
        return clDefaultModuleLoader(vm, spec, res);
    }
}

void printer(CLVM* vm, CLStr str) {
    if (str.len == 1 && str.ptr[0] == '\n') {
        // Skip second invocation by `print` for the new line character.
        return;
    }
    printf("My print: %.*s\n", (int)str.len, str.ptr);
}

int main() {
    // clVerbose = true;
    CLVM* vm = clCreate();
    clSetModuleLoader(vm, modLoader);
    clSetPrinter(vm, printer);

    // Initialize var array for loader.
    vars[0] = (NameValue){"MyConstant", clFloat(1.23)};
    CLValue myInt = clInteger(123);
    vars[1] = (NameValue){"MyList", clNewListDyn(vm, &myInt, 1)};

    CLStr main = STR(
        "use m 'my_mod'\n"
        "\n"
        "var a = 1.0\n"
        "print m.add(a, 2)\n"
        "print(-m.MyConstant)\n"
        "\n"
        "m.MyList.append(3)\n"
        "print m.MyList.len()\n"
        "\n"
        "-- Instantiate a new MyNode.\n"
        "var n = m.MyNode.new(1, 2)\n"
        "dump n.asList()"
    );
    CLValue resv;
    CLResultCode res = clEval(vm, main, &resv);
    if (res == CL_SUCCESS) {
        printf("Success!\n");
    } else {
        CLStr s = clNewLastErrorSummary(vm);
        printf("%.*s\n", (int)s.len, s.ptr);
        clFree(vm, s);
    }
    clRelease(vm, vars[1].v);
    clDeinit(vm);

    // Check that all references were accounted for. (Should be 0)
    printf("Live objects: %zu\n", clCountObjects(vm));
    clDestroy(vm);

    return 0;
}