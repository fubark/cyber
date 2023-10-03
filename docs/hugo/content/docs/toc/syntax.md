---
title: "Syntax"
weight: 1
---

# Syntax
Cyber's syntax is concise and easy to read.

## Statements.
A statement ends with the new line.
```cy
-- An assignment statement.
var a = 123
```
## Blocks.
Some statements can start a new block with a colon.
The first statement in a new block must be indented further.
Spaces or tabs can be used for indentation but not both.
```cy
-- This `if` statement begins a new block.
if true:
    var a = 234
```
Subsequent statements in the block must follow the same indentation.
The block ends when a statement recedes from this indentation.
```cy
for items each it:
    if it == 20:
        print it
        print it + 10
    print it      -- This is the first statement outside of the `if` block.
```
Single-line blocks allow only one statement after a starting block.
```cy
-- A single line block.
if true: print 123

if true: print 123
    -- This is an indentation error since the single-line block is already consumed.
    print 234
```
Since blocks require at least one statement, use `pass` as a placeholder statement.
```cy
func foo():
    pass
```

## Variables.
In Cyber, there are local variables and static variables. The following sections show how variables are declared with the dynamic type.

For declaring typed variables, see [Typed variables]({{<relref "/docs/toc/type-system#typed-variables">}}) and [`auto` declarations]({{<relref "/docs/toc/type-system#auto-declarations">}}).

### Local Variables.
Local variables exist until the end of their scope.
They are declared and initialized using the `var` keyword.
```cy
-- Declaration.
var a = 123

-- Subsequent assignment.
a = 234
```

A new variable can be declared in function blocks with the same name as a variable from a parent block.
```cy
var a = 123
foo = func():
    -- A new local `a` inside function `foo`.
    var a = 234
foo()
print a      -- '123'
```

However, variables declared in sub-blocks such as `if` and `for` can not shadow variables in the same main/function block.
```cy
var a = 123
if true:
    -- CompileError, `a` is already declared in the main block.
    var a = 234 
```

When a parent local is referenced in a [lambda function]({{<relref "/docs/toc/functions#lambdas">}}), the variable is automatically captured. Note that [static functions]({{<relref "/docs/toc/functions#static-functions">}}) can not capture parent locals.

> _Incomplete: Only variables one parent block away can be captured._

```cy
var a = 123
var foo = func():
    a = 234
foo()
print a      -- '234'
```

### Static Variables.
Static variables live until the end of the script.
They act as global variables and are visible from anywhere in the script. 

Static variables are also declared with `var` but `:` is used instead of `=` to initialize a value to them.
```cy
var a: 123
func foo():
    print a     -- '123'
```

Static variables are always exported from the current script. You can read more about exports and [Modules]({{<relref "/docs/toc/modules">}}).

When declared in functions, static variables are initialized once and continue to exist for subsequent function calls.
> _Planned Feature_
```cy
func add(a):
    var sum: 0
    sum += a
    return sum
print add(5)     -- '5'
print add(5)     -- '10'
```

Since static variable declarations are initialized outside of a fiber's execution flow, they can not reference any local variables.
```cy
var a = 123
var b: a      -- Compile error, initializer can not reference a local variable.
```

However, you can reassign any value to them with an assignment statement.
```cy
var a = 123
var b: 0
b = a         -- Reassigning can reference a local variable.
```

Static variable initializers have a natural order based on when it was encountered by the compiler.
In the case of [imported]({{<relref "/docs/toc/modules#importing">}}) variables, the order of the import would affect this order.
The following would print '123' before '234'
```cy
var a: print(123)
var b: print(234)
```

When the initializers reference other static variables, those child references are initialized first in DFS order and supersede the natural ordering. The following initializes `b` before `a`.
```cy
var a: b + 321
var b: 123
print a        -- '444'
```

Circular references in initializers are allowed.
When initialization encounters a reference that creates this circular dependency, that reference evaluates to `none`.
In the following, `a` attempts to initialize first because of its natural ordering. Since `b` is a dependency, it supersedes the natural ordering.
When `b` is found to reference an already visited `a` (causing the circular dependency), it evaluatues to `a`'s current value which is `none`. At the end of initialization, both `a` and `b` have the value `none`.
```cy
var a: b
var b: a
```

Sometimes, you may want to initialize a static variable by executing multiple statements in order.
For this use case, you can use a declaration block.
> _Planned Feature_
```cy
var myImage:
    var img = loadImage('me.png')
    img.resize(100, 100)
    img.filter(.blur, 5)
    break img
```
The final resulting value that is assigned to the static variable is provided by a `break` statement. If a `break` statement is not provided, `none` is assigned instead.

## Keywords.
There are currently `34` keywords in Cyber. This list categorizes them and shows you when you might need them.

- [Control Flow]({{<relref "/docs/toc/control-flow">}}): `if` `else` `match` `case` `while` `for` `each` `break` `continue` `pass` `some`
- [Operators](#operators): `or` `and` `not` `is`
- [Variables](#variables): `var` `as` `auto`
- [Functions]({{<relref "/docs/toc/functions">}}): `func` `return`
- [Coroutines]({{<relref "/docs/toc/concurrency#fibers">}}): `coinit` `coyield`, `coresume`
- [Data Types]({{<relref "/docs/toc/data-types">}}): `type` `object` `enum` `true` `false` `none`
- [Error Handling]({{<relref "/docs/toc/errors">}}): `try` `catch` `error` `throw`
- [Modules]({{<relref "/docs/toc/modules">}}): `import`

## Operators.
Cyber supports the following operators. They are ordered from highest to lowest precedence.
|Operator|Description|
|--|--|
| `<<` `>>` | Bitwise left shift, right shift. |
| `&` | Bitwise and. |
| `\|` `\|\|` | Bitwise or, exclusive or. |
| `^` | Power. |
| `/` `%` `*` | Division, modulus, multiplication. |
| `+` `-` | Addition, subtraction. |
| `as` | Type casting. |
| `>` `>=` `<` `<=` `!=` `==` | Greater, greater or equal, less, less or equal, not equals, equals. |
| `and` | Logical and. |
| `or` | Logical or. |

### Arithmetic Operators.
The following arithmetic operators are supported for the [numeric data types]({{<relref "/docs/toc/data-types#numbers">}}).
```cy
1 + 2     -- Addition, evaluates to 3.
100 - 10  -- Subtraction, evaluates to 90.
3 * 4     -- Multiplication, evaluates to 12.
20 / 5    -- Division, evaluates to 4.
2 ^ 4     -- Raise to the power, evaluates to 16.
12 % 5    -- Modulus remainder, evaluates to 2.
-(10)     -- Apply negative, evaluates to -10.
```

### Comparison Operators.

Cyber supports the following comparison operators.
By default, a comparison operator evaluates to a [Boolean]({{<relref "/docs/toc/data-types#booleans">}}) value.

The equals operator returns true if the two values are equal. For primitive types, the comparison checks the types and the underlying value. For strings, the underlying bytes are compared for equality. For objects, the comparison checks that the two values reference the same object.
```cy
1 == 1      -- Evaluates to `true`
1 == 2      -- Evaluates to `false`
1 == true   -- Evaluates to `false`

var a = 'abc'
a == 'abc'  -- Evaluates to `true`

a = []
b = a
a == b      -- Evaluates to `true`
a == []     -- Evaluates to `false`
```

The not equals operator returns true if the two values are not equal.
```cy
1 != 1      -- Evaluates to `false`
1 != 2      -- Evaluates to `true`
```

Number types have additional comparison operators.
```cy
a > b    -- `true` if a is greater than b
a >= b   -- `true` if a is greater than or equal to b
a < b    -- `true` if a is less than b
a <= b   -- `true` if a is less than or equal to b
```

### Logic Operators.

The logical operators `and`, `or`, and `not` are supported.

`and` evaluates to `a` if `a` is not truthy. Otherwise, it evaluates to `b`. If `a` is not truthy, the evaluation of `b` is not executed. A numeric value that isn't 0 is truthy. An object reference is always truthy. The none value is not truthy.
```cy
true and true  -- Evaluates to true
123 and 234    -- Evaluates to 234
123 and 0      -- Evaluates to false
```
`or` evaluates to `a` if `a` is truthy. Otherwise, it evaluates to `b`. If `a` is found to be truthy, the evaluation of `b` is not executed.
```cy
true or false  -- Evaluates to true
false or true  -- Evaluates to true
false or false -- Evaluates to false
123 or false   -- Evaluates to 123
```

The unary operator `not` performs negation on the boolean value. The unary operator `!` can also be used instead of `not`.
```cy
not false     -- Evaluates to true
not true      -- Evaluates to false
not 0         -- Evaluates to true      
not 123       -- Evaluates to false
!false        -- Evaluates to true
!true         -- Evaluates to false
```

### Bitwise Operators.

The following bitwise operators are supported for `int` number values.
```cy
-- Bitwise and: any underlying bits that are set in both integers are set in the new integer.
a & b

-- Bitwise or: any underlying bits that are set in either integer a or integer b are set in the new integer.
a | b

-- Bitwise exclusive or: any underlying bits that are set in either integer a or integer b but not both are set in the new integer.
a || b

-- Bitwise right shift: a's bits are shifted b bits to the least significant end. This performs sign-extension on the 32-bit integer.
a >> b

-- Bitwise left shift: a's bits are shifted b bits to the most significant end. This does not perform sign-extension on the 32-bit integer.
a << b

-- Bitwise not: a's integer bits are flipped.
~a
```

### Operator Overloading.
See [Operator Overloading]({{<relref "/docs/toc/metaprogramming#operator-overloading">}}) in Metaprogramming.

## Comments.
A single line comment starts with two hyphens and ends at the end of the line.
```cy
-- This is a comment.

var a = 123   -- This is a comment on the same line as a statement.
```
There will be multi-line comments in Cyber but the syntax has not been determined.

## CYON.
CYON or the Cyber object notation is similar to JSON. The format uses the same literal value semantics as Cyber.
```cy
{
    name: 'John Doe'
    'age': 25
    -- This is a comment
    cities: [
        'New York'
        'San Francisco'
        'Tokyo'
    ]
}
```