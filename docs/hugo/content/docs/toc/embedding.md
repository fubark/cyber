---
title: "Embedding"
bookFlatSection: true
weight: 11
---

# Embed API.
The Embed API allows embedding the Cyber compiler and VM as a library into applications. Cyber's core types and the CLI app were built using the Embed API.

The API is defined in the [C header file](https://github.com/fubark/cyber/blob/master/src/include/cyber.h).
The examples shown below can be found in the repository under [c-embedded](https://github.com/fubark/cyber/blob/master/examples/c-embedded). C is used as the host language, but it can be easily translated to C++ or any C-ABI compatible language.

Types from the Embed API begin with `Cs`, constants begin with `CS`, and functions begin with `cs`.

## Getting started.

### Create VM.
Most operations are tied to a VM handle. To create a new VM instance, call `csCreate`:
```c
#include "cyber.h"

int main() {
    CsVM* vm = csCreate();
    // ...
    csDestroy(vm);
    return 0;
}
```

### Override `print`.
The builtin `print` function does nothing by default, so it needs to be overrided to print to stdout for example:
```c
void print(CsVM* vm, CsStr str) {
    printf("My print: %.*s\n", (int)str.len, str.buf);
}

int main() {
    // ...
    csSetPrint(vm, print);
    // ...
}
```

### Eval script.
`csEval` compiles and evaluates a script:
```c
CsStr src = STR(
    "var a = 1\n"
    "print(a + 2)\n"
);

CsValue val;
int res = csEval(vm, src, &val);
if (res == CS_SUCCESS) {
    printf("Success!\n");
    csRelease(vm, val);
} else {
    const char* report = csNewLastErrorReport(vm);
    printf("%s\n", report);
    csFreeStrZ(report);
}
```
If a value is returned from the main block of the script, it's saved to the result value argument.
Memory is managed by ARC so a value that points to a heap object requires a `csRelease` when it's no longer needed.

`csEval` returns a result code that indicates whether it was successful.

## Module Loader.
A module loader describes how a module is loaded when an `import` statement is encountered during script execution.
Only one module loader can be active and is set using `csSetModuleLoader`:
```c
bool modLoader(CsVM* vm, CsStr spec, CsModuleLoaderResult* out) {
    if (strncmp("my_mod", spec.buf, spec.len) == 0) {
        out->src =
            "@host func add(a float, b float) float\n"
            "@host var Root.MyConstant float\n"
            "@host var Root.MyList     List\n"
            "\n"
            "@host\n"
            "type MyCollection object:\n"
            "    @host func asList() any"
            "\n"
            "@host func MyCollection.new(a, b) MyCollection\n";
        out->funcLoader = funcLoader;
        out->varLoader = varLoader;
        out->typeLoader = typeLoader;
        return true;
    } else {
        // Fallback to the default module loader to load `builtins`.
        return csDefaultModuleLoader(vm, spec, out);
    }
}

int main() {
    //...
    csSetModuleLoader(vm, modLoader);
    //...
}
```
The above example checks whether "my_mod" was imported and returns it's source code. Additional loaders are returned to load the functions, variables, and types from the source code.

### Default module loader.
Since only one module loader can be set to the VM instance, a custom loader is required to handle the "builtins" import which contains all of the core types and functions in Cyber. This can simply be delegated to `csDefaultModuleLoader`.

### Function loader.
A function loader describes how to load a `@host` function when it's encountered by the compiler.
The loader can bind functions and type methods:
```c
struct { char* n; CsFuncFn fn; } funcs[] = {
    {"add", add},
    {"asList", myCollectionAsList},
    {"MyCollection.new", myCollectionNew},
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
```
This example uses the `CsFuncInfo.idx` of a @host function to index into an array and return a [Host function](#host-functions) pointer. The name is also compared to ensure it's binding to the correct pointer.

This is an efficient way to map Cyber functions to host functions. A different implementation might use a hash table to map the name of the function to it's pointer.

### Variable loader.
A variable loader describes how to load a `@host` variable when it's encountered by the compiler:
```c
// C has limited static initializers (and objects require a vm instance) so initialize them in `main`.
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

int main() {
    // ...

    // Initialize var array for loader.
    vars[0] = (NameValue){"Root.MyConstant", csFloat(1.23)};
    CsValue myInt = csInteger(123);
    vars[1] = (NameValue){"Root.MyList", csNewList(vm, &myInt, 1)};

    // ...
}
```
This example uses the same technique as the function loader, but it can be much simpler. It doesn't matter how the mapping is done as long as the variable loader returns a `CsValue`.

### Type loader.
A type loader describes how to load a `@host` type when it's encountered by the compiler:
```c
CsTypeId myCollectionId;

bool typeLoader(CsVM* vm, CsTypeInfo info, CsTypeResult* out) {
    if (strncmp("MyCollection", info.name.buf, info.name.len) == 0) {
        out->type = CS_TYPE_OBJECT;
        out->data.object.outTypeId = &myCollectionId;
        out->data.object.getChildren = myCollectionGetChildren;
        out->data.object.finalizer = myCollectionFinalizer;
        return true;
    } else {
        return false;
    }
}
```
When binding to the "MyCollection" type, it's typeId is saved to `outTypeId`. This id is then used to create new instances of this type. See [Host types](#host-types).

## Host functions.
A host function requires a specific function signature:
```c
CsValue add(CsVM* vm, const CsValue* args, uint8_t nargs) {
    double res = csAsFloat(args[0]) + csAsFloat(args[1]);
    return csFloat(res);
}
```
A host function should always return a `CsValue`. `csNone()` can be returned if the function does not intend to return any value.

## Host types.
A host type are types that are opaque to Cyber scripts but still behave like an object. They can have type functions and methods.

Only the host application can directly create new instances of them, so usually a function is binded to expose a constructor to the user script:
```c
// Binding a C struct with it's own children and finalizer.
// This struct retains 2 VM values and has 2 arbitrary data values unrelated to the VM.
typedef struct MyCollection {
    CsValue val1;
    CsValue val2;
    int a;
    double b;
} MyCollection;

// Implement the `new` function in MyCollection.
CsValue myCollectionNew(CsVM* vm, const CsValue* args, uint8_t nargs) {
    // Instantiate our object.
    CsValue new = csNewHostObject(vm, myCollectionId, sizeof(MyCollection));
    MyCollection* my = (MyCollection*)csAsHostObject(new);

    // Assign the constructor args passed in and retain them since the new object now references them.
    csRetain(vm, args[0]);
    my->val1 = args[0];
    csRetain(vm, args[1]);
    my->val2 = args[1];

    // Assign non VM values.
    my->a = 123;
    my->b = 9999.999;
    return new;
}
```
`csNewHostObject` takes the type id (returned from the [Type loader](#type-loader)) and size (in bytes) and returns a new heap object. Note that the size is allowed to vary. Different instances of the same type can occupy different amounts of memory.

### `getChildren`
Since `MyCollection` contains `CsValue` children, the [Type loader](#type-loader) requires a `getChildren` callback so that memory management can reach them:
```c
CsValueSlice myCollectionGetChildren(CsVM* vm, void* obj) {
    MyCollection* my = (MyCollection*)obj;
    return (CsValueSlice){ .ptr = &my->val1, .len = 2 };
}
```

### `finalizer`
A type finalizer is optional since the memory and children of an instance will be freed automatically by ARC.
However, it can be useful to perform additional cleanup tasks for instances that contain external resources.
```c
void myCollectionFinalizer(CsVM* vm, void* obj) {
    printf("MyCollection finalizer was called.\n");
}
```