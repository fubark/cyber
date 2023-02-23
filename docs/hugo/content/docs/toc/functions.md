---
title: "Functions"
weight: 4
---

# Functions.
In Cyber, there are first-class functions (or function values) as well as statically defined functions.

## Static Functions.
Static functions are not initally values themselves. They allow function calls to be optimal since they don't need to resolve a dynamic value. A nice feature of Cyber's static functions is they can be used just like a function value.

Static functions are declared with the `func` keyword and must have a name.
```cy
import m 'math'

func dist(x0, y0, x1, y1):
    dx = x0-x1
    dy = y0-y1
    return m.sqrt(dx^2 + dy^2)
```
Calling static functions is straightforward. You can also reassign or pass them around as values.
```cy
print dist(0, 0, 10, 20)

-- Assigning to a local variable.
bar = dist

-- Passing `dist` as an argument.
func squareDist(dist, size):
    return dist(0, 0, size, size)
print squareDist(dist, 30)
```

The function declaration can also be initialized to an expression that evaluates to a function. However, the expression can not contain any local variable references since it's a static declaration. The function signatures also have to match.
```cy
func myAdd(a, b):
    return a + b
func add(a, b) = myAdd      -- Valid declaration.

myInc = func(a):
    return a + 1
func inc(a) = myInc         -- CompileError, referencing local variable `myInc`.

func foo(a, b, c) = myAdd   -- panic, signature mismatch.
```

Functions can return multiple values.
```cy
import {cos, sin} 'math'

func compute(rad):
    return cos(rad), sin(rad)
x, y = compute(pi)
```

## Function Overloading.
Static functions can be overloaded by the number of parameters in its signature.
```cy
func foo():
    return 2 + 2

func foo(n):
    return 10 + n

func foo(n, m):
    return n * m

print foo()         -- "4"
print foo(2)        -- "12"
print foo(20, 5)    -- "100"
```

## Lambdas.
Lambdas or function values can be assigned to variables or passed as arguments into other constructs.

When a lambda only returns an expression, it can be declared with a simplified syntax.
```cy
-- Passing simple lambda as an argument.
foo(word => toUpper(word))

-- A simple lambda with multiple arguments.
foo((word, prefix) => prefix + toUpper(word))

-- Assigning a simple lambda.
canvas.onUpdate = delta_ms => print delta_ms
```

Lambdas that need a block of statements can be declared with the `func` keyword without a name.
```cy
-- Assigning lambda block to a variable.
add = func (a, b):
    return a + b

-- Passing a lambda block as an argument.
canvas.onUpdate():
    ..func (delta_ms):
        print delta_ms
```
Passing a lambda block as a call argument is only possible in a call expression block. To understand how that works, see [Function Calls](#function-calls).

## Closures.
In Cyber, lambdas can capture local variables in parent blocks. This example shows the lambda `f` capturing `a` from the main scope.
```cy
a = 1
f = func():
    return a + 2
print f()         -- "3"
```

In the following, the `a` referenced in the lambda expression is captured from the static function `add`'s scope.
```cy
func add():
    a = 123
    return b => a + b
addTo = add()
addTo(10)         -- "133"
```

However, static functions can not capture local variables.
```cy
a = 1
func foo():
    print a       -- Compile Error: Can't reference local from static function.
```

## Function Calls.
The straightforward way to call a function is to use parentheses.

```cy
d = dist(100, 100, 200, 200)
```

You can call functions with named parameters.
```cy
d = dist(x0: 10, x1: 20, y0: 30, y1: 40)
```

The shorthand method for calling functions omits parentheses and commas. This only works for functions that accept parameters:
```cy
d = dist 100 100 200 200  -- Calls the function `dist`.

func random():            -- Function with no parameters.
    return 4

r = random                -- Returns the function itself as a value. Does not call the function `random`.
r = random()              -- Calls the function `random`.
```

The top level arguments for the shorthand convention must be separated by whitespace. A string can contain whitespace since it's surrounded by delimiters. 
```cy
a = myFunc 'cyber script'
```

The following has a binary expression with spaces inbetween which is not allowed. Removing that whitespace fixes the call expression.

```cy
a = myFunc 1 + 2     -- Not allowed.
a = myFunc 1+2       -- Correct.
```

Wrapping arguments in parentheses allows you to keep the whitespace in the sub-expression.
```cy
-- This calls the function `myFunc` with 2 arguments.
a = myFunc 'hello' (1 + 2 * 3)

-- Nested function call using the shorthand convention.
a = myFunc 'hello' (otherFunc 1+2 'world')
```

The call expression block continues to add arguments from the block's body. If arguments are omitted from the initial call expression they can be added inside using the `..` syntax. Arguments mapped to named parameters have a key value syntax separated by a `:`. All other arguments are added into a list and passed as the last argument.
```cy
foo(123):
    ..func ():
        return 123
    param3: 123
    234
    bar()
    'hello'
```
In the example above, the function `foo` is called with 4 arguments. The first argument `123` is included in the starting call expression. The second argument is a function value inside the call expression block. The third argument is mapped to the param `param3`. Finally, the fourth argument is a list that contains `234`, `bar()`, and `'hello'`. 