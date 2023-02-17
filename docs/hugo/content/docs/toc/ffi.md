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

lib = os.bindLib('mylib.so', [
    os.CFunc{ sym: 'add', args: [#int, #int], ret: #int }
])
lib.add(123, 321)
```

If the path argument to `bindLib` is just a filename, the search steps for the library is specific to the operating system. Provide an absolute (eg. '/foo/mylib.so') or relative (eg. './mylib.so') path to load from a direct location instead. When the path argument is `none`, it loads the currently running executable as a library allowing you to bind exported functions from the Cyber CLI or your own embedded Cyber app/runtime.

When using `CFunc` or `CStruct` declarations, [tag literals]({{<relref "/docs/toc/data-types#tags">}}) are used to represent default type mappings from Cyber to C and back:
| Binding | Cyber | C | Details |
| -- | -- | -- | -- |
| #bool | bool | bool |
| #char | number | int8_t, signed char | 
| #uchar | number | uint8_t, unsigned char | 
| #short | number | int16_t, short | 
| #ushort | number | uint16_t, unsigned short | 
| #int | number | int32_t, int |
| #uint | number | uint32_t, unsigned int |
| #long | number | int64_t, long long | 
| #ulong | number | uint64_t, unsigned long long | 
| #usize | number | size_t, uintptr_t | 
| #float | number | float |
| #double | number | double |
| #charPtrZ | any | char* (null terminated) | C receives a null terminated string that lives until the end of the function call. |
| #charPtrZ (return) | rawstring | char* (null terminated) |
| #dupeCharPtrZ | any | char* (null terminated) | C receives and owns a null terminated string. |
| #ptr | opaque | void* |
| sym symbol | object \<sym\> | Struct | The mapping from a Cyber object type `sym` and the C-struct can be declared with `CStruct`. |

By default `bindLib` returns an anonymous object with the binded C-functions as methods. This is convenient for using it like an object, but it's less optimal compared to binding as functions. If a config is passed into `bindLib` as the third argument, `genMap: true` makes `bindLib` return a map instead with the binded C-functions as Cyber functions.
The resulting object of `bindLib` holds a reference to an internal TCCState which owns the loaded JIT code.
Once the object is released by ARC, the TCCState is also released which removes the JIT code from memory.

## CFunc.
The `CFunc` object lets you bind to a C-function. The `sym` field maps to the C-function's symbol name in the dynamic library. The `args` field declares the type mapping from Cyber to C-function's arguments. Finally, the `ret` field declares the type mapping from the C-function's return type to a Cyber type.

```cy
import os 'os'

lib = os.bindLib('mylib.so', [
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

object MyObject
    a number
    b string
    c bool

lib = os.bindLib('mylib.so', [
    os.CFunc{ sym: 'foo', args: [MyObject], ret: MyObject }
    os.CStruct{ fields: [#f64, #charPtrZ, #bool], type: MyObject }
])
res = lib.foo(MyObject{ a: 123, b: 'foo', c: true })
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

lib = os.bindLib('mylib.so', [
    os.CFunc{ sym: 'foo', args: [MyObject], ret: #ptr }
    os.CStruct{ fields: [#f64, #charPtrZ, #bool], type: MyObject }
])
ptr = lib.foo(MyObject{ a: 123, b: 'foo', c: true })
res = lib.ptrToMyObject(ptr)
```