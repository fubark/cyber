---
title: "Type System"
bookFlatSection: true
weight: 9
---

# Type System.
Cyber supports gradual typing which allows the use of both dynamically and statically typed code.

Dynamic typing can reduce the amount of friction when writing code, but it can also result in more runtime errors.
Gradual typing allows you to add static typing incrementally which provides compile-time guarantees and prevents runtime errors.
Static typing also makes it easier to maintain and refactor your code.

## Dynamic typing.
A variable with the `any` type can hold any value. It can only be copied to destinations that also accept the `any` type. An `any` value can be used as the callee for a function call or the receiver for a method call. It can be used with any operators.

## Compile-time dynamic typing.
Cyber introduces the concept of compile-time dynamic typing. This allows a local variable to gain additional compile-time features while using it as a dynamic value. It can prevent inevitable runtime errors and avoid unnecessary type casts.

Local variables declared without a type specifier start off with the type of their initializer. In the following, `a` is implicity declared as a `number` at compile-time because number literals default to the `number` type.
```cy
var a = 123
```

The type can change at compile-time from another assignment. 
If `a` is then assigned to a string literal, `a` from that point on becomes the `string` type at compile-time.
```cy
var a = 123
foo(a)           -- Valid call expression.
a = 'hello'
foo(a)           -- CompileError. Expected `number` argument, got `string`.

func foo(n number):
    pass
```

The type of `a` can also change in branches. However, after the branch block, `a` will have a merged type determined by the types assigned to `a` from the two branched code paths. Currently, the `any` type is used if the types from the two branches differ. At the end of the following `if` block, `a` assumes the `any` type after merging the `number` and `string` types.
```cy
var a = 123
if a > 20:
    a = 'hello'
    foo(a)       -- Valid call expression. `foo` can be called without type casting.

foo(a)           -- CompileError. Expected `string` argument, got `any`.

func foo(s string):
    pass
```

## Default types.
Static variables without a type specifier will always default to the `any` type. In the following, `a` is compiled with the `any` type despite being initialized to a number literal.
```cy
var a: 123
a = 'hello'
```

Function parameters without a type specifier will default to the `any` type. The return type also defaults to `any`. In the following, both `a` and `b` have the `any` type despite being only used for arithmetic. 
```cy
func add(a, b):
    return a + b

print add(3, 4) 
```

## Static typing.
In Cyber, types can be optionally declared with variables, parameters, and return values.
The following builtin types are available in every namespace: `bool`, `number`, `int`, `string`, `list`, `map`, `error`, `fiber`, `any`.

A `type object` declaration creates a new object type.
```cy
type Student object:    -- Creates a new type named `Student`
    name string
    age int
    gpa number
```

When a type specifier follows a variable name, it declares the variable with the type. Any operation afterwards that violates the type constraint will result in a compile error.
```cy
a number = 123
a = 'hello'        -- CompileError. Type mismatch.
```

Parameter and return type specifiers in a function signature follows the same syntax.
```cy
func mul(a number, b number) number:
    return a * b

print mul(3, 4)
print mul(3, '4')  -- CompileError. Function signature mismatch.
```

Type specifiers must be resolved at compile-time.
```cy
type Foo object:
    a number
    b string
    c Bar          -- CompileError. Bar is not declared.
```

Circular type references are allowed.
```cy
type Node object:
    val any
    next Node      -- Valid type specifier.
```

## Type aliases.
A type alias is declared from a single line `type` statement. This creates a new type symbol for an existing data type.
```cy
import util './util.cy'

type Vec3 util.Vec3

var v = Vec3{ x: 3, y: 4, z: 5 }
```

## Type casting.
The `as` keyword can be used to cast a value to a specific type. Casting lets the compiler know what the expected type is and does not perform any conversions.
If the compiler knows the cast will always fail at runtime, a compile error is returned instead.
If the cast fails at runtime, a panic is returned.
```cy
print('123' as number)    -- CompileError. Can not cast `string` to `number`.

erased any = 123
add(1, erased as number)  -- Success.

print(erased as string)   -- Panic. Can not cast `number` to `string`.

func add(a number, b number):
    return a + b
```

## Runtime type checking.
Since Cyber allows invoking `any` function values, the callee's function signature is not always known at compile-time. To ensure type safety in this situation, type checking is done at runtime and with no additional overhead compared to calling an untyped function.
```cy
op any = add
print op(1, 2)           -- '3'
print op(1, '2')         -- Panic. Function signature mismatch.

func add(a number, b number) number:
    return a + b
```