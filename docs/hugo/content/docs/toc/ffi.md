---
title: "FFI"
weight: 6
---

# FFI.
Cyber supports binding to an existing C ABI compatible library at runtime.
This allows you to call into dynamic libraries created in C or other languages.
Cyber uses `libtcc` to JIT compile the bindings so function calls are fast.

## FFI context.
An FFI context contains declarations that map C to Cyber. Afterwards, it allows you to bind to a dynamic library or create interoperable objects. To create a new `FFI` context:
```cy
import os

var ffi = os.newFFI()
```

## Declare functions.
Functions from a library are first declared using `cfunc` which accepts C types in the form of symbols. In a future update they will accept C syntax instead.
```cy
ffi.cfunc('add', [.int, .int], .int)
```
The first argument refers to the symbol's name in the dynamic library. 
The second argument contains the function's parameter types and finally the last argument is the function's return type.

The example above maps to this C function:
```c
int add(int a, int b) {
    return a + b;
}
```

## Bind library.
`bindLib` accepts the path to the library and returns a object which can be used to invoke the functions declared from `cfunc`:
```cy
my lib = ffi.bindLib('./mylib.so')
lib.add(123, 321)
```
Note that `my` is used to allow `lib` to be used dynamically since the type is unknown at compile-time.

### Search path.
If the path argument to `bindLib` is just a filename, the search steps for the library is specific to the operating system. Provide an absolute (eg. '/foo/mylib.so') or relative (eg. './mylib.so') path to load from a direct location instead. When the path argument is `none`, it loads the currently running executable as a library allowing you to bind exported functions from the Cyber CLI or your own application/runtime.

### Configuration.
By default `bindLib` returns an anonymous object with the binded C-functions as methods. This is convenient for invoking functions using the method call syntax. If a config is passed into `bindLib` as the second argument, `genMap: true` makes `bindLib` return a map instead with the binded C-functions as Cyber functions.

### Finalizer.
The resulting object of `bindLib` holds a reference to an internal TCCState which owns the loaded JIT code.
Once the object is released by ARC, the TCCState is also released which removes the JIT code from memory.

## Mappings
When using `cfunc` or `cbind` declarations, [symbols]({{<relref "/docs/toc/data-types#symbols">}}) are used to represent default type mappings from Cyber to C and back:
> _Incomplete: This is not the final API for dynamically loading and interfacing with C libraries. The plan is to parse a subset of C headers to bind to Cyber types and functions._

| Binding | Cyber | C | Details |
| -- | -- | -- | -- |
| .bool | bool | bool |
| .char | int | int8_t, signed char | 
| .uchar | int | uint8_t, unsigned char | 
| .short | int | int16_t, short | 
| .ushort | int | uint16_t, unsigned short | 
| .int | int | int32_t, int |
| .uint | int | uint32_t, unsigned int |
| .long | int | int64_t, long long | 
| .ulong | int | uint64_t, unsigned long long | 
| .usize | int | size_t, uintptr_t | 
| .float | float | float |
| .double | float | double |
| .charPtr | pointer | char* | Use `os.cstr()` and `pointer.fromCstr()` to convert between a Cyber string and a null terminated C string.
| .voidPtr | pointer | void* |
| type {S} object | type {S} object | struct | The mapping from a Cyber object type `S` and the C-struct can be declared with `cbind`. |

## Bind to Cyber type.
`cbind` is used to bind a C struct to a Cyber object type. Once declared, the Cyber type can be used as a binding type in function declarations:
```cy
import os

type MyObject object:
    var a float
    var b pointer
    var c bool

ffi.cbind(MyObject, [.float, .voidPtr, .bool])
ffi.cfunc('foo', [MyObject], MyObject)
my lib = ffi.bindLib('./mylib.so')

var res = lib.foo([MyObject a: 123.0, b: os.cstr('foo'), c: true])
```

The example above maps to these C declarations in `mylib.so`:
```c
typedef struct MyObject {
    double a;
    char* b;
    bool c;
} MyObject;

MyObject foo(MyObject o) {
    // Do something.
}
```

`cbind` also generates `ptrTo[Type]` as a helper function to dereference an opaque ptr to a new Cyber object:
```cy
ffi.cfunc('foo', [MyObject], .voidPtr)
my lib = ffi.bindLib('./mylib.so')

var ptr = lib.foo([MyObject a: 123, b: os.cstr('foo'), c: true])
var res = lib.ptrToMyObject(ptr)
```

## Pointers
A `pointer` is used to read or write to an exact memory address. This is typically used for FFI to manually map Cyber types to C, and back. See [`type pointer`]({{<relref "/docs/toc/modules#type-pointer">}}).

A new pointer can be created with the builtin `pointer`.
```cy
var ptr = pointer(0xDEADBEEF)
print ptr.value()     --'3735928559'
```