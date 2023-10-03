---
title: "Type System"
bookFlatSection: true
weight: 9
---

# Type System.
Cyber supports the use of both dynamically and statically typed code.

## Dynamic typing.
Dynamic typing can reduce the amount of friction when writing code, but it can also result in more runtime errors.

### `dynamic` vs `any`
Variables without a type specifier are implicitly assigned the `dynamic` type.
`dynamic` values can be freely used and copied without any compile errors (if there is a chance it can succeed at runtime, see [Recent type inference](#recent-type-inference)):
```cy
var a = 123

func getFirstRune(s string):
    return s[0]

getFirstRune(a)       -- RuntimeError. Expected `string`.
```
Since `a` is dynamic, passing it to a typed function parameter is allowed at compile-time, but will fail when the function is invoked at runtime.

The `any` type on the otherhand is a **static type** and must be explicitly declared as a variable's type specifier:
```cy
var a any = 123

func getFirstRune(s string):
    return s[0]

getFirstRune(a)       -- CompileError. Expected `string`.
```
This same setup will now fail at compile-time because `any` does not satisfy the destination's `string` type constraint.

The use of the `dynamic` type effectively defers type checking to runtime while `any` is a static type and must adhere to type constraints at compile-time.

A `dynamic` value can be used in any operation. It can be invoked as the callee, invoked as the receiver of a method call, or used with operators.

### Invoking `dynamic` values.
When a `dynamic` value is invoked, checks on whether the callee is a function is deferred to runtime. 
```cy
var op = 123
print op(1, 2, 3)      -- RuntimeError. Expected a function.
```

### Dynamic return value.
When the return type of a function is not specified, it defaults to the `dynamic` type.
This allows copying the return value to a typed destination without casting:
```cy
func getValue():
    return 123

func add(a int, b int):
    return a + b

print add(getValue(), 2)    -- Prints "125"
```
The `add` function defers type checking of `getValue()` to runtime because it has the `dynamic` type.

### Recent type inference.
Although a `dynamic` variable has the most flexibility, in some situations it is advantageous to know what type it could be.

The compiler keeps a running record of a `dynamic` variable's most **recent type** to gain additional compile-time features without sacrificing flexibility. It can prevent inevitable runtime errors and avoid unnecessary type casts.

When a `dynamic` variable is first initialized, it has a recent type inferred from its initializer. In the following, `a` has the recent type of `int` at compile-time because numeric literals default to the `int` type:
```cy
var a = 123
```

The recent type can change at compile-time from another assignment. 
If `a` is then assigned to a string literal, `a` from that point on has the recent type of `string` at compile-time:
```cy
var a = 123
foo(a)           -- Valid call expression.
a = 'hello'
foo(a)           -- CompileError. Expected `int` argument, got `string`.

func foo(n int):
    pass
```
Even though `a` is `dynamic` and is usually allowed to defer type checking to runtime, the compiler knows that doing so in this context would **always** result in a runtime error, so it provides a compile error instead. This provides a quicker feedback to fix the problem.

The recent type of `a` can also change in branches. However, after the branch block, `a` will have a recent type after merging the types assigned to `a` from the two branched code paths. Currently, the `any` type is used if the types from the two branches differ. At the end of the following `if` block, `a` has the recent type of `any` type after merging the `int` and `string` types:
```cy
var a = 123
if a > 20:
    a = 'hello'
    foo(a)       -- Valid call expression. `foo` can be called without type casting.

foo(a)           -- CompileError. Expected `string` argument, got `any`.

func foo(s string):
    pass
```

## Static typing.
Static typing can be incrementally applied which provides compile-time guarantees and prevents runtime errors.
Static typing also makes it easier to maintain and refactor your code.
> _Incomplete: Static types in general is in development. One of the goals of Cyber is to let dynamic code mix with typed code. At the moment, there are places where it works and other places where it won't. Keep that in mind when using types._

### Builtin types.
The following builtin types are available in every module: `boolean`, `float`, `int`, `string`, `List`, `Map`, `error`, `fiber`, `any`.

### Typed variables.
A typed local variable can be declared by attaching a type specifier after its name. The value assigned to the variable must satisfy the type constraint or a compile error is issued.
> _Incomplete: Only function parameter and object field type specifiers have meaning to the VM at the moment. Variable type specifiers have no meaning and will be discarded._
```cy
var a float = 123

var b int = 123.0    -- CompileError. Expected `int`, got `float`.
```

Any operation afterwards that violates the type constraint of the variable will result in a compile error.
```cy
a = 'hello'          -- CompileError. Expected `float`, got `string`.
```

Static variables are declared in a similar way except `:` is used instead of `=`:
```cy
var global Map: {}
```

Type specifiers must be resolved at compile-time.
```cy
var foo Foo = none   -- CompileError. Type `Foo` is not declared.
```

### `auto` declarations.
The `auto` declaration creates a new variable with the type inferred from the initializer. 
> _Planned Feature_
```cy
-- Initialized as an `int` variable.
auto a = 123     
```

`auto` declarations are strictly for static typing. If the assigned value's type is `dynamic`, the variable's type becomes `any`.
```cy
func getValue():
    return ['a', 'list']

-- Initialized as an `any` variable.
auto a = getValue()
```

### Object types.
A `type object` declaration creates a new object type. Field types are optional and declared with a type specifier after their name.
```cy
type Student object:    -- Creates a new type named `Student`
    name string
    age  int
    gpa  float
```

Circular type dependencies are allowed.
```cy
type Node object:
    val  any
    next Node      -- Valid type specifier.
```

Instantiating a new object does not require typed fields to be initialized. Missing field values will default to their [zero value](#zero-values):
```cy
var s = Student{}
print s.name       -- Prints ""
print s.age        -- Prints "0"
print s.gpa        -- Prints "0.0"
```

Invoking the zero initializer is not allowed for circular dependencies:
```cy
var n = Node{}     -- CompileError. Can not zero initialize `next`
                   -- because of circular dependency.
```

### Zero values.
The following shows the zero values of builtin or created types.
|Type|Zero value|
|--|--|
|`boolean`|`false`|
|`int`|`0`|
|`float`|`0.0`|
|`string`|`''`|
|`rawstring`|`''`|
|`List`|`[]`|
|`Map`|`{}`|
|`type S object`|`S{}`|
|`@host type S object`|`S.$zero()`|
|`dynamic`|`none`|
|`any`|`none`|

### Type aliases.
A type alias is declared from a single line `type` statement. This creates a new type symbol for an existing data type.
```cy
import util './util.cy'

type Vec3 util.Vec3

var v = Vec3{ x: 3, y: 4, z: 5 }
```

### Functions.
Function parameter and return type specifiers follows a similiar syntax.
```cy
func mul(a float, b float) float:
    return a * b

print mul(3, 4)
print mul(3, '4')  -- CompileError. Function signature mismatch.
```

### Traits.
> _Planned Feature_

### Union types.
> _Planned Feature_

### `any` type.
A variable with the `any` type can hold any value, but copying it to narrowed type destination will result in a compile error:
```cy
func square(i int):
    return i * i

var a any = 123
a = ['a', 'list']         -- Valid assignment to a value with a different type.
a = 10

print square(a)           -- CompileError. Expected `int`, got `any`.
```
`a` must be explicitly casted to satisfy the type constraint:
```cy
print square(a as int)    -- Prints "100".
```

### Invoking `any` values.
Since `any` is a static type, invoking an `any` value must be explicitly casted to the appropriate function type.
> _Planned Feature: Casting to a function type is not currently supported._

```cy
func add(a int, b int) int:
    return a + b

var op any = add
print op(1, 2)         -- CompileError. Expected `func (int, int) any`

auto opFunc = op as (func (int, int) int)
print opFunc(1, 2)     -- Prints "3".
```

### Type casting.
The `as` keyword can be used to cast a value to a specific type. Casting lets the compiler know what the expected type is and does not perform any conversions.

If the compiler knows the cast will always fail at runtime, a compile error is returned instead.
```cy
print('123' as int)       -- CompileError. Can not cast `string` to `int`.
```

If the cast fails at runtime, a panic is returned.
```cy
var erased any = 123
add(1, erased as int)     -- Success.
print(erased as string)   -- Panic. Can not cast `int` to `string`.

func add(a int, b int):
    return a + b
```