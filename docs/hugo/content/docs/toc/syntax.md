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
for items -> it:
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

### Local variables.
Local variables exist until the end of their scope.
They are declared and initialized using the `var` keyword:
```cy
var a = 123
```
When declared without a type specifier next to the variable, it infers the type from the right initializer.
To declare variables for a specific type, see [Typed variables]({{<relref "/docs/toc/type-system#typed-variables">}}). 

Variables can be set afterwards using the `=` operator:
```cy
a = 234
```

### Dynamically typed.
Dynamically typed variables are easier to work with and there is no friction when using them. They are declared using the `my` keyword:
```cy
my a = 123
```
To understand more about dynamically and statically typed code, see [Type System]({{<relref "/docs/toc/type-system">}}).

### Variable scopes.
Blocks create a new variable scope. Variables declared in the current scope will take precedence over any parent variables with the same name:
```cy
func foo():
    var a = 234

    if true:
        var a = 345     -- New `a` declared.
        print a         -- Prints "345"

    print a             -- Prints "234"
```

### Static Variables.
Static variables live until the end of the script.
They act as global variables and are visible from anywhere in the script. 

They are declared with `var` but a namespace must be provided before the variable name:
```cy
var Root.a = 123

func foo():
    print a     -- '123'
```
The `Root` symbol is used to reference the current module's namespace.


Since static variables are initialized outside of a fiber's execution flow, they can not reference any local variables:
```cy
-- Static declaration.
var Root.b = a   -- Compile error, initializer can not reference a local variable.

-- Main execution.
var a = 123
```

However, they can be reassigned after initialization:
```cy
var Root.b = 0

var a = 123
b = a            -- Reassigning after initializing.
```

Static variable initializers have a natural order based on when it was encountered by the compiler.
In the case of [imported]({{<relref "/docs/toc/modules#importing">}}) variables, the order of the import would affect this order.
The following would print '123' before '234':
```cy
var Root.a = print(123)
var Root.b = print(234)
```

When the initializers reference other static variables, those child references are initialized first in DFS order and supersede the natural ordering. The following initializes `b` before `a`.
```cy
var Root.a = b + 321
var Root.b = 123

print a        -- '444'
```

Circular references in initializers are not allowed.
When initialization encounters a reference that creates a circular dependency an error is reported.
```cy
var Root.a = b
var Root.b = a       -- CompileError. Referencing `a` creates a circular dependency.
```

Sometimes, you may want to initialize a static variable by executing multiple statements in order.
For this use case, you can use a declaration block. {{<todo "*Planned Feature">}}
```cy
var Root.myImage =:
    var img = loadImage('me.png')
    img.resize(100, 100)
    img.filter(.blur, 5)
    break img
```
The final resulting value that is assigned to the static variable is provided by a `break` statement. If a `break` statement is not provided, `none` is assigned instead.

## Reserved identifiers.

### Keywords.
There are `26` general keywords. This list categorizes them:

- [Control Flow]({{<relref "/docs/toc/control-flow">}}): `if` `else` `switch` `case` `while` `for` `break` `continue` `pass`
- [Operators](#operators): `or` `and` `not` `is`
- [Variables](#variables): `var` `my`
- [Functions]({{<relref "/docs/toc/functions">}}): `func` `return`
- [Coroutines]({{<relref "/docs/toc/concurrency#fibers">}}): `coinit` `coyield`, `coresume`
- [Data Types]({{<relref "/docs/toc/data-types">}}): `type` `as`
- [Error Handling]({{<relref "/docs/toc/errors">}}): `try` `catch` `throw`
- [Modules]({{<relref "/docs/toc/modules">}}): `import`

### Contextual keywords.
These keywords only have meaning in a certain context.
- [Methods]({{<relref "/docs/toc/data-types#methods">}}): `self`, `Self`
- [Catching Errors]({{<relref "/docs/toc/errors#caught-variable">}}): `caught`
- [Object Type]({{<relref "/docs/toc/data-types#objects">}}): `object`
- [Enum Type]({{<relref "/docs/toc/data-types#enums">}}): `enum`
- [Function Throws]({{<relref "/docs/toc/errors#throws-specifier">}}): `throws`

### Symbols.
- [Modules]({{<relref "/docs/toc/modules">}}): `Root`

### Literals.
- [Booleans]({{<relref "/docs/toc/data-types#booleans">}}): `true` `false`
- [Error Values]({{<relref "/docs/toc/errors#error-value">}}): `error`
- [None]({{<relref "/docs/toc/data-types#none">}}): `none`

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