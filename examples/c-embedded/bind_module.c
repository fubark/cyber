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
struct { char* n; CsHostFuncFn fn; } funcs[] = {
    {"add", add},
};

bool funcLoader(CsVM* vm, CsHostFuncInfo funcInfo, CsHostFuncResult* out) {
    // Check that the name matches before setting the function pointer.
    if (strncmp(funcs[funcInfo.idx].n, funcInfo.name.buf, funcInfo.name.len) == 0) {
        out->ptr = funcs[funcInfo.idx].fn;
        return true;
    } else {
        return false;
    }
}

// C has limited static initializers (and objects need a vm instance) so initialize them in `main`.
typedef struct { char* n; CsValue v; } NameValue;
NameValue vars[2];

bool varLoader(CsVM* vm, CsHostVarInfo varInfo, CsValue* out) {
    // Check that the name matches before setting the value.
    if (strncmp(vars[varInfo.idx].n, varInfo.name.buf, varInfo.name.len) == 0) {
        // Objects are consumed by the module.
        *out = vars[varInfo.idx].v;
        return true;
    } else {
        return NULL;
    }
}

bool modLoader(CsVM* vm, CsStr spec, CsModuleLoaderResult* out) {
    if (strncmp("my_engine", spec.buf, spec.len) == 0) {
        out->src = STR(
            "@host func add(a float, b float) float\n"
            "@host var MyConstant float\n"
            "@host var MyList List\n"
        );
        out->srcIsStatic = true;
        out->funcLoader = funcLoader;
        out->varLoader = varLoader;
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
    vars[1] = (NameValue){"MyList", csNewList(vm)};

    CsStr main = STR(
        "import mod 'my_engine'\n"
        "\n"
        "var a = 1.0\n"
        "print mod.add(a, 2)\n"
        "print(-mod.MyConstant)\n"
        "mod.MyList.append(3)\n"
        "print mod.MyList.len()\n"
    );
    CsValue resv;
    CsResultCode res = csEval(vm, main, &resv);
    if (res == CS_SUCCESS) {
        printf("Success!\n");
    } else {
        CsStr err = csAllocLastErrorReport(vm);
        PRINTS(err);
        csFreeStr(vm, err);
    }
    csDeinit(vm);

    // Check that all references were accounted for. (Should be 0)
    printf("Live objects: %zu\n", csCountObjects(vm));
    csDestroy(vm);

    return 0;
}