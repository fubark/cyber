---
title: "FFI"
weight: 6
---

# FFI.
Cyber supports binding to an existing C ABI compatible library at runtime.
This allows you to call into dynamic libraries created in C or other languages.
Cyber uses `libtcc` to JIT compile the bindings so function calls are fast. `bindLib` is part of the `os` module and accepts the path to the library as a string and a list of [CFunc](#cfunc) or [CStruct](#cstruct) declarations.

```cy
import os 'os'

var lib = os.bindLib('mylib.so', [
    os.CFunc{ sym: 'add', args: [#int, #int], ret: #int }
])
lib.add(123, 321)
```

If the path argument to `bindLib` is just a filename, the search steps for the library is specific to the operating system. Provide an absolute (eg. '/foo/mylib.so') or relative (eg. './mylib.so') path to load from a direct location instead. When the path argument is `none`, it loads the currently running executable as a library allowing you to bind exported functions from the Cyber CLI or your own embedded Cyber app/runtime.

When using `CFunc` or `CStruct` declarations, [symbols]({{<relref "/docs/toc/data-types#symbols">}}) are used to represent default type mappings from Cyber to C and back:
| Binding | Cyber | C | Details |
| -- | -- | -- | -- |
| #bool | bool | bool |
| #char | float | int8_t, signed char | 
| #uchar | float | uint8_t, unsigned char | 
| #short | float | int16_t, short | 
| #ushort | float | uint16_t, unsigned short | 
| #int | float | int32_t, int |
| #uint | float | uint32_t, unsigned int |
| #long | float | int64_t, long long | 
| #ulong | float | uint64_t, unsigned long long | 
| #usize | float | size_t, uintptr_t | 
| #float | float | float |
| #double | float | double |
| #charPtr | pointer | char* | Use `os.cstr()` and `os.fromCstr()` to convert between a Cyber string and a null terminated C string.
| #voidPtr | pointer | void* |
| sym symbol | object \<sym\> | Struct | The mapping from a Cyber object type `sym` and the C-struct can be declared with `CStruct`. |

By default `bindLib` returns an anonymous object with the binded C-functions as methods. This is convenient for using it like an object, but it's less optimal compared to binding as functions. If a config is passed into `bindLib` as the third argument, `genMap: true` makes `bindLib` return a map instead with the binded C-functions as Cyber functions.
The resulting object of `bindLib` holds a reference to an internal TCCState which owns the loaded JIT code.
Once the object is released by ARC, the TCCState is also released which removes the JIT code from memory.

## CFunc.
The `CFunc` object lets you bind to a C-function. The `sym` field maps to the C-function's symbol name in the dynamic library. The `args` field declares the type mapping from Cyber to C-function's arguments. Finally, the `ret` field declares the type mapping from the C-function's return type to a Cyber type.

```cy
import os 'os'

var lib = os.bindLib('mylib.so', [
    os.CFunc{ sym: 'add', args: [#int, #int], ret: #int }
])
lib.add(123, 321)
```
The example above maps to this C declaration in `mylib.so`:
```text
int add(int a, int b) {
    return a + b;
}
```

## CStruct.
You can also bind object types to C-structs using the `CStruct` object. The `type` field accepts an object type symbol and `fields` indicates the mapping for each field in `type` to and from a C-struct.
After adding a `CStruct` declaration, you can use the object type symbol in CFunc `args` and `ret` and also other CStruct `fields`.
```cy
import os 'os'

type MyObject object:
    a float
    b pointer
    c bool

var lib = os.bindLib('mylib.so', [
    os.CFunc{ sym: 'foo', args: [MyObject], ret: MyObject }
    os.CStruct{ fields: [#f64, #charPtr, #bool], type: MyObject }
])
var res = lib.foo(MyObject{ a: 123, b: os.cstr('foo'), c: true })
```
The example above maps to these C declarations in `mylib.so`:
```text
typedef struct MyObject {
    double a;
    char* b;
    bool c;
} MyObject;

MyObject foo(MyObject o) {
    // Do something.
}
```

`CStruct` also generates `ptrTo[Type]` as a helper function to dereference an opaque ptr to a new Cyber object:
```cy
import os 'os'

var lib = os.bindLib('mylib.so', [
    os.CFunc{ sym: 'foo', args: [MyObject], ret: #voidPtr }
    os.CStruct{ fields: [#f64, #charPtr, #bool], type: MyObject }
])
var ptr = lib.foo(MyObject{ a: 123, b: os.cstr('foo'), c: true })
var res = lib.ptrToMyObject(ptr)
```

## Pointers
A `pointer` is used to read or write to an exact memory address. This is typically used for FFI to manually map Cyber types to C, and back. A new pointer can be created with the builtin `pointer`.
```cy
var ptr = pointer(0xDEADBEEF)
print ptr.value()     --'3735928559'
```

### `type pointer`
| Method | Summary |
| ------------- | ----- |
| `value() int` | Returns the memory address as an `int`. The value may be negative since it's bitcasted from an unsigned 48-bit integer but it retains the original pointer bits. |