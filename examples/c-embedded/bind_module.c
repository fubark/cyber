#include <string.h>
#include <stdio.h>
#include "cyber.h"

// This example shows how to setup a module loader for custom modules.
// Declarations from a module's source code invokes callbacks to bind with the host's functions and types.
// To see how to inject symbols programmatically, see `inject_module.c`.

// Compile this program with a C compiler. `zig cc` is used here as an example.
// zig cc bind_module.c -I ../../src/include ../../zig-out/lib/libcyber.a -o main.exe

// Convenience macros to deal with Cyber string slices.
#define STR(s) ((CsStr){ s, strlen(s) })
#define PRINTS(s) (printf("%.*s\n", (int)s.len, s.buf))

CsValue add(CsVM* vm, const CsValue* args, uint8_t nargs) {
    double res = csAsFloat(args[0]) + csAsFloat(args[1]);
    return csFloat(res);
}

// Forward declaration.
CsValue myCollectionNew(CsVM* vm, const CsValue* args, uint8_t nargs);
CsValue myCollectionAsList(CsVM* vm, const CsValue* args, uint8_t nargs);

struct { char* n; CsFuncFn fn; } funcs[] = {
    {"add", add},
    {"new", myCollectionNew},
    {"asList", myCollectionAsList},
};

bool funcLoader(CsVM* vm, CsFuncInfo info, CsFuncResult* out) {
    // Check that the name matches before setting the function pointer.
    if (strncmp(funcs[info.idx].n, info.name.buf, info.name.len) == 0) {
        out->ptr = funcs[info.idx].fn;
        return true;
    } else {
        return false;
    }
}

// C has limited static initializers (and objects need a vm instance) so initialize them in `main`.
typedef struct { char* n; CsValue v; } NameValue;
NameValue vars[2];

bool varLoader(CsVM* vm, CsVarInfo info, CsValue* out) {
    // Check that the name matches before setting the value.
    if (strncmp(vars[info.idx].n, info.name.buf, info.name.len) == 0) {
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
    CsValue val1;
    CsValue val2;
    int a;
    double b;
} MyCollection;

CsTypeId myCollectionId;

// Implement the `new` function in MyCollection.
CsValue myCollectionNew(CsVM* vm, const CsValue* args, uint8_t nargs) {
    // Instantiate our object.
    CsValue new = csNewHostObject(vm, myCollectionId, sizeof(MyCollection));
    MyCollection* my = (MyCollection*)csAsHostObject(new);

    // Assign the constructor args passed in and retain them since our object now
    // has shared ownership.
    csRetain(vm, args[0]);
    my->val1 = args[0];
    csRetain(vm, args[1]);
    my->val2 = args[1];

    // Assign non VM values.
    my->a = 123;
    my->b = 9999.999;
    return new;
}

// Implement the `asList` method in MyCollection.
CsValue myCollectionAsList(CsVM* vm, const CsValue* args, uint8_t nargs) {
    // First argument is `self`.
    MyCollection* my = (MyCollection*)csAsHostObject(args[0]);

    CsValue vals[4] = {my->val1, my->val2, csInteger(my->a), csFloat(my->b)};
    return csNewList(vm, &vals[0], 4);
}

CsValueSlice myCollectionGetChildren(CsVM* vm, void* obj) {
    MyCollection* my = (MyCollection*)obj;
    return (CsValueSlice){ .ptr = &my->val1, .len = 2 };
}

void myCollectionFinalizer(CsVM* vm, void* obj) {
    printf("MyCollection finalizer was called.\n");
}

bool typeLoader(CsVM* vm, CsTypeInfo info, CsTypeResult* out) {
    if (strncmp("MyCollection", info.name.buf, info.name.len) == 0) {
        out->data.object.outTypeId = &myCollectionId;
        out->data.object.getChildren = myCollectionGetChildren;
        out->data.object.finalizer = myCollectionFinalizer;
        return true;
    } else {
        return false;
    }
}

// This module loader provides the source code and callbacks to load @host funcs, vars, and types.
bool modLoader(CsVM* vm, CsStr spec, CsModuleLoaderResult* out) {
    if (strncmp("my_engine", spec.buf, spec.len) == 0) {
        out->src = STR(
            "@host func add(a float, b float) float\n"
            "@host var MyConstant float\n"
            "@host var MyList List\n"
            "\n"
            "@host\n"
            "type MyCollection object:\n"
            "  @host func new(a, b) MyCollection\n"
            "  @host func asList(self) any"
        );
        out->srcIsStatic = true;
        out->funcLoader = funcLoader;
        out->varLoader = varLoader;
        out->typeLoader = typeLoader;
        return true;
    } else {
        // Fallback to the default module loader to load `builtins`.
        return csDefaultModuleLoader(vm, spec, out);
    }
}

void print(CsVM* vm, CsStr str) {
    printf("MY_VM: %.*s\n", (int)str.len, str.buf);
}

int main() {
    // csVerbose = true;
    CsVM* vm = csCreate();
    csSetModuleLoader(vm, modLoader);
    csSetPrint(vm, print);

    // Initialize var array for loader.
    vars[0] = (NameValue){"MyConstant", csFloat(1.23)};
    CsValue myInt = csInteger(123);
    vars[1] = (NameValue){"MyList", csNewList(vm, &myInt, 1)};

    CsStr main = STR(
        "import eng 'my_engine'\n"
        "\n"
        "var a = 1.0\n"
        "print eng.add(a, 2)\n"
        "print(-eng.MyConstant)\n"
        "\n"
        "eng.MyList.append(3)\n"
        "print eng.MyList.len()\n"
        "\n"
        "-- Instantiate a new MyCollection.\n"
        "var myc = eng.MyCollection.new(1, 2)\n"
        "dump myc.asList()"
    );
    CsValue resv;
    CsResultCode res = csEval(vm, main, &resv);
    if (res == CS_SUCCESS) {
        printf("Success!\n");
    } else {
        CsStr err = csNewLastErrorReport(vm);
        PRINTS(err);
        csFreeStr(vm, err);
    }
    csDeinit(vm);

    // Check that all references were accounted for. (Should be 0)
    printf("Live objects: %zu\n", csCountObjects(vm));
    csDestroy(vm);

    return 0;
}