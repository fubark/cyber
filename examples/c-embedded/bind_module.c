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
CLValue myCollectionNew(CLVM* vm, const CLValue* args, uint8_t nargs);
CLValue myCollectionAsList(CLVM* vm, const CLValue* args, uint8_t nargs);

struct { char* n; CLFuncFn fn; } funcs[] = {
    {"add", add},
    {"asList", myCollectionAsList},
    {"MyCollection.new", myCollectionNew},
};

bool funcLoader(CLVM* vm, CLFuncInfo info, CLFuncResult* out) {
    // Check that the name matches before setting the function pointer.
    if (strncmp(funcs[info.idx].n, info.name.ptr, info.name.len) == 0) {
        out->ptr = funcs[info.idx].fn;
        return true;
    } else {
        return false;
    }
}

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
typedef struct MyCollection {
    CLValue val1;
    CLValue val2;
    int a;
    double b;
} MyCollection;

CLTypeId myCollectionId;

// Implement the `new` function in MyCollection.
CLValue myCollectionNew(CLVM* vm, const CLValue* args, uint8_t nargs) {
    // Instantiate our object.
    CLValue new = clNewHostObject(vm, myCollectionId, sizeof(MyCollection));
    MyCollection* my = (MyCollection*)clAsHostObject(new);

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

// Implement the `asList` method in MyCollection.
CLValue myCollectionAsList(CLVM* vm, const CLValue* args, uint8_t nargs) {
    // First argument is `self`.
    MyCollection* my = (MyCollection*)clAsHostObject(args[0]);

    CLValue vals[4] = {my->val1, my->val2, clInteger(my->a), clFloat(my->b)};
    return clNewList(vm, &vals[0], 4);
}

CLValueSlice myCollectionGetChildren(CLVM* vm, void* obj) {
    MyCollection* my = (MyCollection*)obj;
    return (CLValueSlice){ .ptr = &my->val1, .len = 2 };
}

void myCollectionFinalizer(CLVM* vm, void* obj) {
    printf("MyCollection finalizer was called.\n");
}

bool typeLoader(CLVM* vm, CLTypeInfo info, CLTypeResult* out) {
    if (strncmp("MyCollection", info.name.ptr, info.name.len) == 0) {
        out->type = CL_BIND_TYPE_CUSTOM;
        out->data.custom.out_type_id = &myCollectionId;
        out->data.custom.get_children = myCollectionGetChildren;
        out->data.custom.finalizer = myCollectionFinalizer;
        return true;
    } else {
        return false;
    }
}

// This module loader provides the source code and callbacks to load #host funcs, vars, and types.
bool modLoader(CLVM* vm, CLStr spec, CLModuleLoaderResult* out) {
    if (strncmp("my_mod", spec.ptr, spec.len) == 0) {
        out->src =
            "#host func add(a float, b float) float\n"
            "#host var .MyConstant float\n"
            "#host var .MyList     List\n"
            "\n"
            "#host\n"
            "type MyCollection:\n"
            "    #host func asList() any"
            "\n"
            "#host func MyCollection.new(a any, b any) MyCollection\n";
        out->srcLen = strlen(out->src);
        out->funcLoader = funcLoader;
        out->varLoader = varLoader;
        out->typeLoader = typeLoader;
        return true;
    } else {
        // Fallback to the default module loader to load `builtins`.
        return clDefaultModuleLoader(vm, spec, out);
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
    vars[1] = (NameValue){"MyList", clNewList(vm, &myInt, 1)};

    CLStr main = STR(
        "use mod 'my_mod'\n"
        "\n"
        "var a = 1.0\n"
        "print mod.add(a, 2)\n"
        "print(-mod.MyConstant)\n"
        "\n"
        "mod.MyList.append(3)\n"
        "print mod.MyList.len()\n"
        "\n"
        "-- Instantiate a new MyCollection.\n"
        "var myc = mod.MyCollection.new(1, 2)\n"
        "dump myc.asList()"
    );
    CLValue resv;
    CLResultCode res = clEval(vm, main, &resv);
    if (res == CL_SUCCESS) {
        printf("Success!\n");
    } else {
        CLStr s = clNewLastErrorSummary(vm);
        printf("%.*s\n", (int)s.len, s.ptr);
        clFreeStr(vm, s);
    }
    clRelease(vm, vars[1].v);
    clDeinit(vm);

    // Check that all references were accounted for. (Should be 0)
    printf("Live objects: %zu\n", clCountObjects(vm));
    clDestroy(vm);

    return 0;
}