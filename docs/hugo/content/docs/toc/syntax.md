---
title: "Syntax"
weight: 1
---

# Syntax
Cyber's syntax is concise while still being easy to read and understand.

## Statements.
In Cyber, a statement ends with the new line.
A statement block is not surrounded by delimiters like braces.
Instead, the beginning of a block starts with a colon.
This is intended to make a clear distinction between expressions and structured statements.
```cy
-- This is a statement.
a = 123

-- This statement begins a new block.
if true:
    a = 234
```
The first statement in a block must be indented further than the block declaration. Spaces or tabs can be used for indentation but not both. The rest of the statements in the block must follow the same indentation. When the next statement recedes from this indentation the block ends.
```cy
for items each it:
    if it == 20:
        print it
        print it
    print it      -- This is the first statement outside of the `if` block.
```
Single line blocks allows you to have one statement on the same line as the block statement.
```cy
-- A single line block.
if true: print 123

if true: print 123
    -- This is an indentation error since the single line block is already consumed.
    print 234
```

## Variables.
In Cyber, there are local variables and static variables.

### Local Variables.
Local variables exist until the end of their scope.
They are set using the assignment statement.
```cy
a = 123
```

Function blocks and the main block have their own local variable scope.
Variable assignments prefer to set the variable in the current block. If there is no such variable in the current block, it will create a new local variable.
```cy
a = 123
foo = func():
    -- A new local `a` inside `foo`.
    a = 234
foo()
print a      -- '123'
```

If the assignment was intended for a parent local variable, the `capture` keyword is used.
```cy
a = 123
foo = func():
    capture a = 234
foo()
print a      -- '234'
```

Once declared with `capture`, any subsequent assignments will also set the parent local variable.
```cy
a = 123
foo = func():
    capture a = 234
    a = 345
foo()
print a      -- '345'
```

Control flow constructs such as `if` or `for` are considered sub-blocks and share the same variable scope as the block they are in.
```cy
a = 123
if true:
    a = 234
print a      -- '234'
```

Referencing a variable that doesn't exist in the current block, will find the first variable above the current block.
```cy
a = 123
foo = func():
    print a
foo()        -- '123'
```

### Static Variables.
Unlike local variables, static variables are always available until the end of the script.
They act as global variables and are visible from anywhere in the script without being captured by a closure. 
You can declare static variables with the `var` keyword.
```cy
var a: 123
func foo():
    print a     -- '123'
```

The initializer comes after a colon instead of an assignment operator when declaring a static variable.

Since assignment statements prefer to write to a variable in it's local block, the `static` keyword is used to select a static variable instead.
```cy
var a: 123
func foo():
    static a = 234
foo()
print a         -- '234'
```

Static variables are also exported from the current script. You can read more about exports and [Modules](#modules).
```cy
var a: 123      -- Exported under the current module's namespace.
```

When declared in functions, static variables are initialized once and continue to exist for subsequent function calls.
```cy
func add(a):
    var sum: 0
    sum += a
    return sum
print add(5)     -- '5'
print add(5)     -- '10'
```

Since static variable declarations are initialized outside of the normal execution flow, they can not reference any local variables.
```cy
a = 123
var b: a      -- Compile error, initializer can not reference a local variable.
```

However, you can reassign any value to them with an assignment statement.
```cy
a = 123
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
When initialization encounters a reference that creates this circular dependency, that reference evaluates to `none` at that moment.
In the following, `a` attempts to initialize first because of its natural ordering. Since `b` is a dependency, it supersedes the natural ordering.
When `b` is found to reference an already visited `a` (causing the circular dependency), it evaluatues to `a`'s current value which is `none`. At the end of initialization, both `a` and `b` have the value `none`.
```cy
var a: b
var b: a
```

Sometimes, you may want to initialize a static variable by executing multiple statements in order.
For this use case, you can use a declaration block.
```cy
var myImage:
    img = loadImage('me.png')
    img.resize(100, 100)
    img.filter(#blur, 5)
    break img
```
The final resulting value that is set to the static variable is provided by a `break` statement. If a `break` statement is not provided, `none` is used instead.

## Keywords.
There are currently `35` keywords in Cyber. This list categorizes them and shows you when you might need them.

- [Control Flow]({{<relref "/docs/toc/control-flow">}}): `if` `then` `else` `match` `while` `for` `each` `break` `continue` `pass` `some`
- [Operators](#operators): `or` `and` `not` `is`
- [Variables](#variables): `var` `static` `capture` `as`
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
Cyber supports the following arithmetic operators. To understand how these operators affect the number type, see [Numbers]({{<relref "/docs/toc/data-types#numbers">}}).
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
A comparison expression always evaluates to a [Boolean]({{<relref "/docs/toc/data-types#booleans">}}) value.

The equals operator returns true if the two values are equal. For primitive types, the comparison checks the types and the underlying value. For strings, the underlying bytes are compared for equality. For objects, the comparison checks that the two values reference the same object. The not equals operator returns true if the two values are not equal.
```cy
1 == 1      -- Evaluates to `true`
1 == 2      -- Evaluates to `false`
1 == true   -- Evaluates to `false`

a = []
b = a
a == b      -- Evaluates to `true`
a == []     -- Evaluates to `false`

1 != 1      -- Evaluates to `false`
1 != 2      -- Evaluates to `true`
```

Numbers have additional comparison operators.
```cy
a > b    -- `true` if a is greater than b
a >= b   -- `true` if a is greater than or equal to b
a < b    -- `true` if a is less than b
a <= b   -- `true` if a is less than or equal to b
```

### Logic Operators.

The logical operators `and`, `or`, and `not` are supported.

`and` evaluates to `a` if `a` is not truthy. Otherwise, it evaluates to `b`. If `a` is not truthy, the evaluation of `b` is not executed. A number value that isn't 0 is truthy. An object reference is always truthy. The none value is not truthy.
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

Cyber supports the following bitwise operators for integer values. Number operands are first converted to a 32-bit integer (two's complement) before applying the operation.
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

## Comments.
A single line comment starts with two hyphens and ends at the end of the line.
```cy
-- This is a comment.

a = 123   -- This is a comment on the same line as a statement.
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