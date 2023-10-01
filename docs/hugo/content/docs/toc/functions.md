---
title: "Functions"
weight: 4
---

# Functions.
In Cyber, there are first-class functions (or function values) and static functions.

## Static Functions.
Static functions are not initially values themselves. They allow function calls to be optimal since they don't need to resolve a dynamic value.

Static functions are declared with the `func` keyword and must have a name.
```cy
import math

func dist(x0, y0, x1, y1):
    var dx = x0-x1
    var dy = y0-y1
    return math.sqrt(dx^2 + dy^2)
```
Calling static functions is straightforward. You can also reassign or pass them around as function values.
```cy
print dist(0, 0, 10, 20)

-- Assigning to a local variable.
var bar = dist

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

var myInc = func(a):
    return a + 1
func inc(a) = myInc         -- CompileError, referencing local variable `myInc`.

func foo(a, b, c) = myAdd   -- panic, signature mismatch.
```

Functions can return multiple values. *This feature has not been confirmed nor implemented.*
```cy
import {cos, sin} 'math'

func compute(rad):
    return cos(rad), sin(rad)
var x, y = compute(pi)
```

## Function Overloading.
Static functions can be overloaded by the number of parameters in its signature. [Typed functions]({{<relref "/docs/toc/type-system#static-typing">}}) are further overloaded by its type signature. 
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
var add = func (a, b):
    return a + b

-- Passing a lambda block as an argument.
canvas.onUpdate():
    ..func (delta_ms):
        print delta_ms
```
Passing a lambda block as a call argument is only possible in a call block. See [Function Calls](#function-calls).

## Closures.
In Cyber, lambdas can capture local variables from parent blocks. This example shows the lambda `f` capturing `a` from the main scope.
```cy
var a = 1
var f = func():
    return a + 2
print f()         -- "3"
```

The following lambda expression captures `a` from the function `add`.
```cy
func add():
    var a = 123
    return b => a + b
var addTo = add()
print addTo(10)   -- "133"
```

However, static functions can not capture local variables.
```cy
var a = 1
func foo():
    print a       -- Compile Error: Can't reference local from static function.
```

## Named Parameters.
> _Planned Feature_

## Optional Parameters.
> _Planned Feature_

## Variadic Parameters.
> _Planned Feature_

## Function Calls.
The straightforward way to call a function is to use parentheses.

```cy
var d = dist(100, 100, 200, 200)
```

You can call functions with named parameters.
> _Planned Feature_
```cy
var d = dist(x0: 10, x1: 20, y0: 30, y1: 40)
```

### Shorthand syntax.
The shorthand method for calling functions omits parentheses and commas. This only works for functions that accept parameters:
> _Incomplete: Only the most trivial cases work with the shorthand method. The case with operators being separated by spaces might not end up being implemented._
```cy
var d = dist 100 100 200 200  -- Calls the function `dist`.

func random():            -- Function with no parameters.
    return 4

var r = random                -- Returns the function itself as a value. Does not call the function `random`.
r = random()              -- Calls the function `random`.
```

The top level arguments for the shorthand convention must be separated by whitespace. A string can contain whitespace since it's surrounded by delimiters. 
```cy
var a = myFunc 'cyber script'
```

The following has a binary expression with spaces inbetween which is not allowed. Removing that whitespace fixes the call expression.

```cy
var a = myFunc 1 + 2     -- Not allowed.
a = myFunc 1+2       -- Correct.
```

Wrapping arguments in parentheses allows you to keep the whitespace in the sub-expression.
```cy
-- This calls the function `myFunc` with 2 arguments.
var a = myFunc 'hello' (1 + 2 * 3)

-- Nested function call using the shorthand convention.
a = myFunc 'hello' (otherFunc 1+2 'world')
```

### Call block syntax.
The call expression block continues to add arguments from the block's body. If arguments are omitted from the initial call expression they can be added inside using the `..` syntax. Arguments mapped to named parameters have a key value syntax separated by a `:`. All other arguments are added into a list and passed as the last argument.
> _Planned Feature_
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