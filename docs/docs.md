# Table of Contents.
- [Introduction.](#introduction)
- [Syntax.](#syntax)
- [Basic Types.](#basic-types)
- [Custom Types.](#custom-types)
- [C Types.](#c-types)
- [Control Flow.](#control-flow)
- [Functions.](#functions)
- [Memory.](#memory)
- [Error Handling.](#error-handling)
- [Concurrency.](#concurrency)
- [Dynamic Typing.](#dynamic-typing)
- [Metaprogramming.](#metaprogramming)
- [Modules.](#modules)
- [FFI.](#ffi)
- [libcyber.](#libcyber)
- [CLI.](#cli)

<!--TOC-END-->

<!--This does not contain module docs. Use docs.md from releases instead or generate it by using docs/gen-docs.cy -->

# Introduction.

Cyber is a fast, efficient, and concurrent scripting language. The landing page is at [cyberscript.dev](https://cyberscript.dev) and contains performance metrics and release notes.

These docs provide a reference manual for the language. You can read it in order or jump around using the navigation. You may come across features that are marked `Incomplete` or `Planned`. This is because the docs are written as if all features have been completed already.

## Type System.
Cyber is a statically typed language so the documentation will provide examples using typed syntax. However, dynamic typing is also supported with a less restrictive syntax. If you're coming from a language such as Python, JavaScript, or Lua, it can be easier to get started with [Dynamic Typing](#dynamic-typing).

## Hello World.
```cy
use math

var worlds = {'World', '世界', 'दुनिया', 'mundo'}
worlds.append(math.random())
for worlds -> w:
    print "Hello, $(w)!"
```

# Syntax.

<table><tr>
<td valign="top">

* [Statements.](#statements)
* [Blocks.](#blocks)
* [Variables.](#variables)
  * [Local variables.](#local-variables)
  * [Explicit type constraint.](#explicit-type-constraint)
  * [Variable scopes.](#variable-scopes)
  * [Static variables.](#static-variables)
  * [Context variables.](#context-variables)
  * [`extern` variables.](#extern-variables)
  * [`use $global`](#use-global)
* [Reserved identifiers.](#reserved-identifiers)
  * [Keywords.](#keywords)
  * [Contextual keywords.](#contextual-keywords)
  * [Literals.](#literals)
</td><td valign="top">

* [Operators.](#operators)
  * [Arithmetic operators.](#arithmetic-operators)
  * [Comparison operators.](#comparison-operators)
  * [Logic operators.](#logic-operators)
  * [Bitwise operators.](#bitwise-operators)
  * [Operator overloading.](#operator-overloading)
* [Zero values.](#zero-values)
* [Type casting.](#type-casting)
* [Comments.](#comments)
* [CYON.](#cyon)
</td>
</tr></table>

[^top](#table-of-contents)

Cyber's syntax is concise and easy to read.

## Statements.
A statement ends with the new line:
```cy
var a = 123
```

To wrap a statement to the next line, the last token before the new line must be an operator or a separator:
```cy
var gameover = health <= 0 or
    player.collidesWith(spikes)

if year > 2020 and year <= 2030 and
    month > 0 and month <= 11:
    print 'Valid'
```

Any token inside a delimited syntax (such as parentheses or braces) can be wrapped to the next line:
```cy
var sum = add(1, 2, 3, 4,
    100, 200, 300, 400)

var colors = {'red', 'blue', 'green',
    'purple', 'orange', 'yellow'}
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
The block ends when a statement recedes from this indentation:
```cy
var items = {10, 20, 30}
for items -> it:
    if it == 20:
        print it
    print it      -- This is the first statement outside of the `if` block.
```
Compact blocks allow only one statement after the starting block:
```cy
-- A single line block.
if true: print 123

if true: print 123
    -- This is an indentation error since the compact block is already consumed.
    print 234
```
Since blocks require at least one statement, use `pass` as a placeholder statement:
```cy
func foo():
    pass
```

## Variables.
Variables allow values to be stored into named locations in memory.

Local variables and static variables are supported.

### Local variables.
Local variables exist until the end of their scope.
They are declared and initialized using the `var` keyword:
```cy
var a = 123
```
When declared without a type specifier next to the variable, the type is inferred from the right initializer. In the above example, the variable `a` is initialized with the type `int`.

Variables can be assigned afterwards using the `=` operator:
```cy
a = 234
```

### Explicit type constraint.
When a type specifier is provided, the value assigned to the variable must satisfy the type constraint or a compile error is reported:
```cy
var a float = 123.0

var b int = 123.0    --> CompileError. Expected `int`, got `float`.
```

Any operation afterwards that violates the type constraint of the variable will result in a compile error
```cy
a = 'hello'          --> CompileError. Expected `float`, got `String`.
```

### Variable scopes.
Blocks create a new variable scope. Variables declared in the current scope will take precedence over any parent variables with the same name:
```cy
func foo():
    var a = 234

    if true:
        var a = 345     -- New `a` declared.
        print a         --> 345

    print a             --> 234
```

### Static variables.
*Static variables will likely be removed in favor of [context variables](#context-variables), constants, and [`extern` variables](#extern-variables).*

Static variables live until the end of the script.
They act as global variables and are visible from anywhere in the script. 

Static variables are declared with `var` like local variables, but a namespace must be provided before the variable name:
```cy
var .a = 123

func foo():
    print a     --> 123
```
The `.` prefix is used to reference the current module's namespace.

Unlike local variables, static variables do not currently infer the type from the right hand side so a specific type must be specified or it will default to the `any` type:
```cy
var .my_map Map = Map{}
```

Since static variables are initialized outside of a fiber's execution flow, they can not reference any local variables:
```cy
var .b = a   --> Compile error, initializer can not reference a local variable.

-- Main execution.
var a = 123
```

However, they can be reassigned after initialization:
```cy
var .b = 0

var a = 123
b = a            -- Reassigning after initializing.
```

Static variable initializers have a natural order based on when it was encountered by the compiler.
In the case of [imported](#importing) variables, the order of the import would affect this order.
The following would print '123' before '234':
```cy
var .a = print(123)
var .b = print(234)
```

When the initializers reference other static variables, those child references are initialized first in DFS order and supersede the natural ordering. The following initializes `b` before `a`.
```cy
var .a = b + 321
var .b = 123

print a        --> 444
```

Circular references in initializers are not allowed.
When initialization encounters a reference that creates a circular dependency an error is reported.
```cy
var .a = b
var .b = a     --> CompileError. Referencing `a` creates a circular dependency.
```

Sometimes, you may want to initialize a static variable by executing multiple statements in order.
For this use case, you can use a declaration block. *Planned Feature*
```cy
var .myImage =:
    var img = loadImage('me.png')
    img.resize(100, 100)
    img.filter(.blur, 5)
    break img
```
The final resulting value that is assigned to the static variable is provided by a `break` statement. If a `break` statement is not provided, `none` is assigned instead.

### Context variables.
Context variables are bound to each virtual thread and are accessible anywhere on the call stack.
All fibers created in the same virtual thread reference the same context variables.

Context variables used in the program must be declared in the main source file with a default value: *Planned Feature*
```cy
-- main.cy

context MyInt = 123
context MyString = 'abc'

print MyInt        --> 123
print MyString     --> abc
```

To reference a context variable in a different source file, redeclare it with the same type:
```cy
-- foo.cy

context MyInt int

func foo():
    print MyInt    --> 123
```
Since this is a redeclaration, an assignment statement is not allowed.

Some context variables such as `mem` for [memory allocations](#memory-allocations) are already declared in every program, so it only needs a redeclaration:
```cy
context mem Memory

var a = mem.new(int)
a.* = 123
print a.*          --> 123
mem.free(a)
```

### `extern` variables.
> _Planned Feature_

### `use $global`
When `use $global` is declared in the module, it allows the use of undeclared variables:
```cy
use $global

a = 123
print a    --> 123
```

Accessing an undeclared variable before it's initialized results in a runtime error:
```cy
use $global

print a    --> panic: `a` is not defined in `$global`.
```

## Reserved identifiers.

### Keywords.
There are `29` general keywords. This list categorizes them:

- [Control Flow](#control-flow): `if` `else` `switch` `case` `while` `for` `break` `continue` `pass`
- [Operators](#operators): `or` `and` `not`
- [Variables](#variables): [`var`](#local-variables) [`context`](#context-variables)
- [Functions](#functions): `func` `return`
- [Fibers](#fibers): `coinit` `coyield` `coresume`
- [Async](#async): `await`
- [Types](#custom-types): `type`  `as`
- [Type embedding](#type-embedding): `use`
- [Error Handling](#error-handling): `try` `catch` `throw`
- [Modules](#modules): `use` `mod`
- [Dynamic Typing](#dynamic-typing): `let`

### Contextual keywords.
These keywords only have meaning in a certain context.
- [Methods](#methods): `self` `Self`
- [Types](#custom-types): [`object`](#objects) [`struct`](#structs) [`cstruct`](#c-structs) [`enum`](#enums) [`trait`](#traits)
- [Catching Errors](#caught-variable): `caught`
- Function Return: `void`

### Literals.
- [Boolean literal](#booleans): `true` `false`
- [Symbol literal](#symbols): `symbol`
- [Error literal](#error-value): `error`
- None: `none`

## Operators.
Cyber supports the following operators. They are ordered from highest to lowest precedence.

|Operator|Description|
|---|---|
| `<<` `>>` | Bitwise left shift, right shift. |
| `&` | Bitwise and. |
| `|` `||` | Bitwise or, exclusive or. |
| `^` | Power. |
| `/` `%` `*` | Division, modulus, multiplication. |
| `+` `-` | Addition, subtraction. |
| `as` | Type casting. |
| `>` `>=` `<` `<=` `!=` `==` | Greater, greater or equal, less, less or equal, not equals, equals. |
| `and` | Logical and. |
| `or` | Logical or. |
| `..` `-..` | Range, reversed range. |

### Arithmetic Operators.
The following arithmetic operators are supported for the [numeric data types](#numbers).
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
By default, a comparison operator evaluates to a [Boolean](#booleans) value.

The equals operator returns true if the two values are equal. For primitive types, the comparison checks the types and the underlying value. For strings, the underlying bytes are compared for equality. For objects, the comparison checks that the two values reference the same object.
```cy
1 == 1      -- Evaluates to `true`
1 == 2      -- Evaluates to `false`
1 == true   -- Evaluates to `false`

var a = 'abc'
a == 'abc'  -- Evaluates to `true`

a = {_}
var b = a
a == b      -- Evaluates to `true`
a == {_}    -- Evaluates to `false`
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

`and` evaluates to `true` if both operands are `true`. Otherwise, it evaluates to `false`. If the left operand is `false`, the evaluation of the right operand is skipped:
```cy
true and true    --> true
true and false   --> false
false and true   --> false
false and false  --> false
```
`or` evaluates to `true` if at least one of the operands is `true`. Otherwise, it evaluates to `false`. If the left operand is `true`, the evaluation of the right operand is skipped:
```cy
true or true     --> true
true or false    --> true
false or true    --> true
false or false   --> false
```

The unary operator `not` performs negation on the boolean value. The unary operator `!` can also be used instead of `not`.
```cy
not false     --> true
not true      --> false
!false        --> true
!true         --> false
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

### Operator overloading.
See [Operator overloading](#operator-overloading-1) in Metaprogramming.

## Zero values.
Uninitialized type fields currently default to their zero values. However, this implicit behavior will be removed in the future in favor of a default value clause. Zero values must then be expressed using the reserved `zero` literal.

The following shows the zero values of builtin or created types:

|Type|Zero value|
|---|---|
|`boolean`|`false`|
|`int`|`0`|
|`float`|`0.0`|
|`String`|`''`|
|`List[T]`|`List[T]{}`|
|`Map`|`Map{}`|
|`type S`|`S{}`|
|`@host type S`|`S.$zero()`|
|`dyn`|`int(0)`|
|`any`|`int(0)`|
|`?S`|`Option[S].none`|

## Type casting.
The `as` keyword can be used to cast a value to a specific type. Casting lets the compiler know what the expected type is and does not perform any conversions.

If the compiler knows the cast will always fail at runtime, a compile error is returned instead.
```cy
print('123' as int)       --> CompileError. Can not cast `String` to `int`.
```

If the cast fails at runtime, a panic is returned.
```cy
var erased any = 123
add(1, erased as int)     --> Success.
print(erased as String)   --> panic: Can not cast `int` to `String`.

func add(a int, b int):
    return a + b
```

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
    name  = 'John Doe',
    'age' = 25,

    -- This is a comment
    cities = {
        'New York',
        'San Francisco',
        'Tokyo',
    },
}
```

# Basic Types.

<table><tr>
<td valign="top">

* [Booleans.](#booleans)
* [Numbers.](#numbers)
  * [Integers.](#integers)
  * [Floats.](#floats)
  * [Big Numbers.](#big-numbers)
* [Strings.](#strings)
  * [Raw string literal.](#raw-string-literal)
  * [String literal.](#string-literal)
  * [Escape sequences.](#escape-sequences)
  * [String indexing.](#string-indexing)
  * [String concatenation.](#string-concatenation)
  * [String interpolation.](#string-interpolation)
  * [String formatting.](#string-formatting)
  * [Line-join literal.](#line-join-literal)
  * [Mutable strings.](#mutable-strings) 
* [Symbols.](#symbols)
</td>
<td valign="top">

* [Optionals.](#optionals)
  * [Wrap value.](#wrap-value)
  * [Wrap `none`.](#wrap-none)
  * [Unwrap or panic.](#unwrap-or-panic)
  * [Unwrap or default.](#unwrap-or-default)
  * [Optional chaining.](#optional-chaining)
  * [`if` unwrap.](#if-unwrap)
  * [`while` unwrap.](#while-unwrap)
* [Arrays.](#arrays)
* [Lists.](#lists)
* [Tables.](#tables)
  * [Table indexing.](#table-indexing)
  * [Check field existence.](#check-field-existence)
  * [Prototypes.](#prototypes)
* [Maps.](#maps)
  * [Map indexing.](#map-indexing)
  * [Map operations.](#map-operations)
  * [Map block.](#map-block)
* [`any`.](#any)
* [`dyn`.](#dyn)
</td>
</tr></table>

[^top](#table-of-contents)

In Cyber, there are value types and reference types.
This section will describe common builtin types.

## Booleans.
Booleans can be `true` or `false`. See [`type bool`](#type-bool).
```cy
var a = true
if true:
    print 'a is true'
```

## Numbers.

### Integers.
`int` is the default integer type and is an alias for `int64`.
It has 64-bits representing integers in the range -(2<sup>63</sup>) to 2<sup>63</sup>-1.
Integer types use the two's complement format.

The following integer types are supported: [`byte`](#type-byte), [`int8`](#type-int8), [`int16`](#type-int16), [`int32`](#type-int32), [`int64`](#type-int64).
Integer types are named according to how many bits they occupy.

When a numeric literal is used and the type can not be inferred, it will default to the `int` type:
```cy
var a = 123
```

Integer notations always produce an integer value:
```cy
var a = 0xFF     -- hex.
a = 0o17         -- octal.
a = 0b1010       -- binary.
a = `🐶`         -- UTF-8 rune.
```

Arbitrary values can be converted to a `int` using the type as a function.
```cy
var a = '123'
var b = int(a) 
```

In addition to arithmetic operations, integers can also perform [bitwise operations](#bitwise-operators).

### Floats.
`float` is the default floating point type. It has a (IEEE 754) 64-bit floating point format. See [`type float`](#type-float).

Although a `float` represents a decimal number, it can also represent integers between -(2<sup>53</sup>-1) and (2<sup>53</sup>-1). Any integers beyond the safe integer range is not guaranteed to have a unique representation.

A numeric literal can be used to create a `float` if the inferred type is a `float`:
```cy
var a float = 123
```

Decimal and scientific notations always produce a `float` value:
```cy
var a = 2.34567
var b = 123.0e4
```

Arbitrary values can be converted to a `float` using the type as a function. 
```cy
var a = '12.3'
var b = float(a) 
```

### Big Numbers.
> _Planned Feature_

## Strings.
The `String` type represents a sequence of UTF-8 codepoints. Each code point is stored internally as 1-4 bytes. See [`type String`](#type-string).

Strings are not validated by default. If the codepoint is invalid at a particular index, the replacement character (0xFFFD) is returned instead.

Strings are **immutable**, so operations that do string manipulation return a new string. By default, short strings are interned to reduce memory footprint.

Under the hood, there are multiple string implementations to make operations faster by default using SIMD.

### Raw string literal.
A raw string doesn't allow any escape sequences or string interpolation.

Single quotes are used to delimit a single line literal:
```cy
var fruit = 'apple'
var str = 'abc🦊xyz🐶'
```

Since raw strings interprets the sequence of characters as is, a single quote character can not be escaped:
```cy
var fruit = 'Miso's apple'    -- ParseError.
```

Triple single quotes are used to delimit a multi-line literal. It also allows single quotes and double single quotes in the string:
```cy
var fruit = '''Miso's apple'''
var greet = '''Hello
World'''
```

### String literal.
A string literal allows escape sequences and string interpolation.

Double quotes are used to delimit a single line literal:
```cy
var fruit = "apple"
var sentence = "The $(fruit) is tasty."
var doc = "A double quote can be escaped: \""
```

Triple double quotes are used to delimit a multi-line literal:
```cy
var title = "last"
var doc = """A double quote " doesn't need to be escaped."""
var str = """line a
line "b"
line $(title)
"""
```

### Escape sequences.
The following escape sequences are supported in string literals wrapped in double quotes:

| Sequence | Code | Character |
| --- | --- | --- |
| `\0` | 0x00 | Null |
| `\a` | 0x07 | Terminal bell |
| `\b` | 0x08 | Backspace |
| `\e` | 0x1b | Escape |
| `\n` | 0x0a | Line feed |
| `\r` | 0x0d | Carriage return |
| `\t` | 0x09 | Horizontal tab |
| `\"` | 0x22 | Double quote |
| `\\` | 0x5c | Backslash |
| `\x??` | -- | Hex number |

Example:
```cy
print "\xF0\x9F\x90\xB6"    --> 🐶
```

### String indexing.
The index operator returns the rune starting at the given byte index:
```cy
var a = 'abcxyz'
print a[1]          --> 98
print(a[1] == `b`)  --> true
```

If an index does not begin a sequence of valid UTF-8 bytes, the replacement character (0xFFFD, 65533) is returned:
```cy
var a = '🐶abcxyz'
print a[1]          --> 65533
```

Since indexing operates at the byte level, it should not be relied upon for iterating runes or rune indexing.
However, if the string is known to only contain ASCII runes (each rune occupies one byte), indexing will return the expected rune.

`String.seek` will always return the correct byte index for a rune index:
```cy
var a = '🐶abcxyz'
print a.seek(2)         --> 5
print a[a.seek(2)]      --> 98 (`b`)
```

Similarily, slicing operates on byte indexes. Using the slice operator will return a view of the string at the given start and end (exclusive) indexes. The start index defaults to 0 and the end index defaults to the string's byte length:
```cy
var str = 'abcxyz'
var sub = str[0..3]
print str[0..3]  --> abc
print str[..5]   --> abcxy
print str[1..]   --> bcxyz
```

### String concatenation.
Concatenate two strings together with the `+` operator or the method `concat`. 
```cy
var res = 'abc' + 'xyz'
res = res.concat('end')
```

### String interpolation.
Expressions can be embedded into string templates with `$()`:
```cy
var name = 'Bob'
var points = 123
var str = "Scoreboard: $(name) $(points)"
```
String templates can not contain nested string templates.

### String formatting.
Values that can be formatted into a string will have a `fmt` method:
```cy
var file = os.openFile('data.bin', .read)
var bytes = file.readToEnd()

-- Dump contents in hex.
print "$(bytes.fmt(.x))"
```

### Line-join literal.
The line-join literal joins string literals with the new line character `\n`. *Planned Feature*

This has several properties:
* Ensures the use of a consistent line separator: `\n`
* Allows lines to have a mix of raw string or string literals.
* Single quotes and double quotes do not need to be escaped.
* Allows each line to be indented along with the surrounding syntax.
* The starting whitespace for each line is made explicit.

```cy
var paragraph = {
    \'the line-join literal
    \'hello\nworld
    \"hello $(name)
    \'last line
    \'
}
```

### Mutable strings.
To mutate an existing string, use [type MutString](#mutstring). *Planned Feature*

## Optionals.
An Optional is a value type that provides **null safety** by forcing the inner value to be unwrapped before it can be used.

The `Option` template type is a choice type that either holds a `none` value or contains `some` value. The option template is defined as:
```cy
type Option[T type] enum:
    case none
    case some T
```
A type prefixed with `?` is the idiomatic way to create an option type. The following String optional types are equivalent:
```cy
Option[String]
?String
```

### Wrap value.
A value is automatically wrapped into the inferred optional's `some` case:
```cy
var a ?String = 'abc'
print a     --> some(abc)'
```

### Wrap `none`.
`none` is automatically initialized to the inferred optional's `none` case:
```cy
var a ?String = none
print a     --> none
```

### Unwrap or panic.
The `.?` access operator is used to unwrap an optional. If the expression evaluates to the `none` case, the runtime panics:
```cy
var opt ?int = 123
var v = opt.?
print v     --> 123
```

### Unwrap or default.
The `?else` control flow operator either returns the unwrapped value or a default value when the optional is `none`:
```cy
var opt ?int = none
var v = opt ?else 123
print v     --> 123
```

`?else` can be used in an assignment block: *Planned Feature*
```cy
var v = opt ?else:
    break 'empty'

var v = opt ?else:
    throw error.Missing
```

### Optional chaining.
Given the last member's type `T` in a chain of `?.` access operators, the chain's execution will either return `Option[T].none` on the first encounter of `none` or returns the last member as an `Option[T].some`: *Planned Feature*
```cy
print root?.a?.b?.c?.last
```

### `if` unwrap.
The `if` statement can be amended to unwrap an optional value using the capture `->` operator:
```cy
var opt ?String = 'abc'
if opt -> v:
    print v     -- Prints 'abc'
```

### `while` unwrap. 
The `while` statement can be amended to unwrap an optional value using the capture `->` operator.
The loop exits when `none` is encountered:
```cy
var iter = dir.walk()
while iter.next() -> entry:
    print entry.name
```

## Arrays.
An array type is a value type and is denoted as `[N]T` where `N` is the size of the array and `T` is the element type.
See [`type Array`](#type-array).

Arrays are initialized with its type followed by the initializer literal:
```cy
var a = [3]int{1, 2, 3}
```

The number of elements can be inferred using pseudo type `[.]T`: *Planned Feature*
```cy
var a = [.]int{1, 2, 3}
```

The array type can be inferred by the dot initializer literal:
```cy
var a [3]int = .{1, 2, 3}
```

Arrays can be indexed:
```cy
a[2] = 300
print a[2]    --> 300
```

## Lists.
Lists are a builtin type that holds an ordered collection of elements. Lists grow or shrink as you insert or remove elements. See [`type List`](#type-list).

The first element of the list starts at index 0.
Lists are initialized with elements in braces:
```cy
var list = {1, 2, 3}
print list[0]    --> 1
```

The empty list is initialized with an underscore as the only element:
```cy
var list = {_}
print list.len() --> 0
```

Lists can be sliced with the range `..` clause. The sliced list becomes a new list that you can modify without affecting the original list. The end index is non-inclusive.
```cy
var list = {1, 2, 3, 4, 5}
print list[0..0]    --> {_}          
print list[0..3]    --> {1, 2, 3}
print list[3..]     --> {4, 5}
print list[..3]     --> {1, 2, 3}
```

The `+..` invokes the slice operator with an end position that is an increment from the start: *Planned Feature*

```cy
var list = {1, 2, 3, 4, 5}
print list[2+..2]   --> {3, 4}
```

List operations.
```cy
var list = {234}

-- Append a value.
list.append(123)

-- Inserting a value at an index.
list.insert(1, 345)

-- Get the length.
print list.len()  -- Prints '2'

-- Sort the list in place.
list.sort((a, b) => a < b)

-- Iterating a list.
for list -> it:
    print it

-- Remove an element at a specific index.
list.remove(1)
```

Since `List` is a generic type, an explicit List type can be attached to the initializer:
```cy
var a = List[int]{1, 2, 3}
```

When the intializer is only prefixed with a dot, it will infer the List type constraint:
```cy
var a List[int] = .{1, 2, 3}
```

## Tables.
A `Table` is a versatile object that can have an arbitrary set of fields.

By default, the record literal initializes a `Table`:
```cy
var o = {}

o = {a=123}
print o.a          --> 123
```

A `Table` can be initialized explicitly using its type name:
```cy
var o = Table{a=123}
```

Any field can be assigned a value.
However, accessing a field before it's initialized results in a panic:
```cy
o.my_field = 234
print o.my_field   --> 234

print o.foo        --> panic: The field `foo` was not initialized.
```

### Table indexing.
Indexing can be used to access a dynamic field or an arbitrary key:
```cy
var o = {name='Nova'}
var field = 'name'
print o[field]     --> Nova

o[10] = [1, 2, 3]
print o[10]        --> List (3)
```
If the key is not an identifier string, the value can only be obtained through indexing.

### Check field existence.
> _Planned Feature_

### Prototypes.
> _Planned Feature_

## Maps.
Maps are a builtin type that store key value pairs in dictionaries. See [`type Map`](#type-map).

Maps are initialized with the `Map` type and a record literal:
```cy
var map = Map{a=123, b=() => 5}
```

The empty record literal creates an empty map:
```cy
var empty = Map{}
```

### Map indexing.
Get a value from the map using the index operator:
```cy
print map['a']
```

### Map operations.
```cy
var map = Map{}

-- Set a key value pair.
map[123] = 234

-- Get the size of the map.
print map.size()

-- Remove an entry by key.
map.remove(123)

-- Iterating a map.
for map -> {val, key}:
    print "$(key) -> $(val)"
```

### Map block.
Entries can also follow a collection literal block.
This gives structure to the entries and has
the added benefit of allowing multi-line lambdas.
*Most likely this will not be implemented in favor of a builder syntax*
```cy
var colors = {}:
    .red   = 0xFF0000
    .green = 0x00FF00
    .blue  = 0x0000FF
    .dump  = func (c):
        print c.red
        print c.green
        print c.blue

    -- Nested map.
    .darker = {}: 
        .red   = 0xAA0000
        .green = 0x00AA00
        .blue  = 0x0000AA
```

## Symbols.
Symbol literals begin with `symbol.`, followed by an identifier.
Each symbol has a global unique ID.
```cy
var currency = symbol.usd
print currency == .usd   --> true
print int(currency)      --> <unique ID>
```

## `any`.
Unlike `dyn`, `any` is statically typed and performs type checks at compile-time.
`any` type can hold any value, but copying it to narrowed type destination will result in a compile error:
```cy
func square(i int):
    return i * i

var a any = 123
a = {'a', 'list'}         --> Valid assignment to a value with a different type.
a = 10

print square(a)           --> CompileError. Expected `int`, got `any`.
```
`a` must be explicitly casted to satisfy the type constraint:
```cy
print square(a as int)    --> 100
```

## `dyn`.
The dynamic type defers type checking to runtime. However, it also tracks its own **recent type** in order to surface errors at compile-time. See [Dynamic Typing](#dynamic-typing).

# Custom Types.
<table><tr>
<td valign="top">

* [Objects.](#objects)
  * [Instantiate.](#instantiate)
  * [Field visibility.](#field-visibility)
  * [Default field values.](#default-field-values)
  * [Circular references.](#circular-references)
  * [Unnamed object.](#unnamed-object)
  * [Methods.](#methods)
  * [Implicit members.](#implicit-members)
  * [Type functions.](#type-functions)
  * [Type variables.](#type-variables)
  * [Type embedding.](#type-embedding)
* [Structs.](#structs)
  * [Declare struct.](#declare-struct)
  * [Copy structs.](#copy-structs)
* [Tuples.](#tuples)
</td><td valign="top">

* [Enums.](#enums)
* [Choices.](#choices)
  * [Initialize choice.](#initialize-choice)
  * [Choice `switch`.](#choice-switch)
  * [Access choice.](#access-choice)
* [Type aliases.](#type-aliases)
* [Distinct types.](#distinct-types)
* [Traits.](#traits)
* [Type templates.](#type-templates)
  * [Expand type template.](#expand-type-template)
</td>
</tr></table>

[^top](#table-of-contents)

## Objects.
An object type contains field and function members. New instances can be created from them similar to a struct or class in other languages. Unlike classes, there is no builtin concept of inheritance but a similar effect can be achieved using composition and embedding.

Object types are declared with `type object`. The `object` keyword is optional when using a `type` declaration. These two declarations are equivalent:
```cy
type A object:
    my_field int

type A:
    my_field int
```

Fields must be declared at the top of the `type` block with their names and type specifiers:
```cy
type Node:
    value int
    next  ?Node
```

### Instantiate.
New object instances are created using a record literal with a leading type name:
```cy
var node = Node{value=123, next=none}
print node.value       -- Prints "123"
```

A dot record literal can also initialize to the inferred object type:
```cy
var node Node = .{value=234, next=none}
print node.value       -- Prints "234"
```

## Field visibility.
All fields have public visibility. However, when a field is declared with a `-` prefix, it suggests that it should not be made available to an editor's autocomplete:
```cy
type Info:
    a       int
    -b      int
    -secret String
```

### Default field values.
When a field is omitted in the record literal, it gets initialized to its [zero value](#zero-values):
```cy
var node Node = .{value=234}
print node.next    --> Option.none

type Student:
    name String
    age  int
    gpa  float

var s = Student{}
print s.name       -->
print s.age        --> 0
print s.gpa        --> 0.0
```

### Circular references.
Circular type references are allowed if the object can be initialized:
```cy
type Node:
    val  any
    next ?Node

var n = Node{}    -- Initializes.
```
In this example, `next` has an optional `?Node` type so it can be initialized to `none` when creating a new `Node` object.

The following example will fail because this version of `Node` can not be initialized:
```cy
type Node:
    val  any
    next Node

var n = Node{}    -- CompileError. Can not zero initialize `next`
                   -- because of circular dependency.
```

### Unnamed object.
Unnamed object types can be declared and used without an explicit `type` declaration:
```cy
type Node:
    value object:
        a  int
        b  float
    next  ?Node

var node = Node{
    value = .{a=123, b=100.0},
    next  = none,
}
```

### Methods.
Methods are functions that are invoked with a parent instance using the `.` operator.
When the first parameter of a function contains `self`, it's declared as a method of the parent type:
```cy
type Node:
    value int
    next  ?Node

    func inc(self, n):
        self.value += n

    func incAndPrint(self):
        self.inc(321)
        print value

var n = Node{value=123, next=none}
n.incAndPrint()         -- Prints "444"
```
`self` is then used to reference members of the parent instance.

Methods can be declared outside of the type declaration as a flat declaration:
```cy
func Node.getNext(self):
    return self.next
```

### Implicit members.
When a method is nested under a type declaration, the type members can be implicitly referenced inside the method: *Incomplete: Only the type's fields can be referenced this way.*
```cy
type Node:
    value int
    next  ?Node

    func double(self):
        return value * 2
```

### Type functions.
Type functions can be declared within the type block without a `self` param:
```cy
type Node:
    value int
    next  ?Node

    func new():
        return Node{value=123, next=none}

var n = Node.new()
```

Type functions can also be declared outside of the type block using a flat declaration:
```cy
type Node:
    value int
    next  ?Node

func Node.new():
    return Node{value=123, next=none}

var n = Node.new()
```

### Type variables.
Similarily, type variables are declared outside of the `type` block:
```cy
-- Declare inside the `Node` namespace.
var Node.DefaultValue = 100

print Node.DefaultValue    -- Prints "100"
```

### Type embedding.
Type embedding facilitates type composition by using the namespace of a child field's type: *Planned Feature*
```cy
type Base:
    a int

    func double(self) int:
        return a * 2

type Container:
    b use Base

var c = Container{b = Base{a=123}}
print c.a         --> 123
print c.double()  --> 246
```
Note that embedding a type does not declare extra fields or methods in the containing type. It simply augments the type's using namespace by binding the embedding field.

If there is a member name conflict, the containing type's member has a higher precedence:
```cy
type Container:
    a int
    b use Base

var c = Container{a=999, b = Base{a=123}}
print c.a         --> 999
print c.double()  --> 246
```

Since the embedding field is named, it can be used just like any other field:
```cy
print c.b.a       --> 123
```

## Structs.
Struct types can contain field and method members just like object types, but their instances are copied by value rather than by reference. In that sense, they behave like primitive data types.

Unlike objects, structs are value types and do not have a reference count. They can be safely referenced with the `ref` modifier and their lifetime can be managed with single ownership semantics. Unsafe pointers can not reference structs by default, but there may be an unsafe builtin to allow it anyway.

### Declare struct.
Struct types are created using the `type struct` declaration:
```cy
type Vec2 struct:
    x float
    y float

var v = Vec2{x=30, y=40}
```

### Copy structs.
Since structs are copied by value, assigning a struct to another variable creates a new struct:
```cy
var v = Vec2{x=30, y=40}
var w = v
v.x = 100
print w.x    -- Prints '30'
print v.x    -- Prints '100'
```

## Tuples.
> _Incomplete: Tuples can only be created from @host funcs at the moment._

## Enums.
A new enum type can be declared with the `type enum` declaration.
An enum value can only be one of the unique symbols declared in the enum type.
By default, the symbols generate unique ids starting from 0.
```cy
type Fruit enum:
    case apple
    case orange
    case banana
    case kiwi

var fruit = Fruit.kiwi
print fruit       -- 'Fruit.kiwi'
print int(fruit)  -- '3'
```
When the type of the value is known to be an enum, it can be assigned using a symbol literal.
```cy
var fruit = Fruit.kiwi
fruit = .orange
print(fruit == Fruit.orange)   -- 'true'
```

## Choices.
Choices are enums with payloads (also known as sum types or tagged unions). An enum declaration becomes a choice type if one of the cases has a payload type specifier:
```cy
type Shape enum:
    case rectangle Rectangle
    case circle    object:
        radius float
    case triangle  object:
        base   float
        height float
    case line      float
    case point 

type Rectangle:
    width  float
    height float
```

### Initialize choice.
The general way to initialize a choice is to invoke the initializer with the payload as the argument:
```cy
var rect = Rectangle{width=10, height=20}
var s = Shape.rectangle(rect)

s = Shape.line(20)
```

If the payload is a record-like type, the choice can be initialized with a record literal:
```cy
var s = Shape.rectangle{width=10, height=20}
```

A choice without a payload is initialized like an enum member:
```cy
var s = Shape.point
```

### Choice `switch`.
`case` clauses can match choices and capture the payload:
```cy
switch s
case .rectangle -> r:
    print "$(r.width) $(r.height)"
case .circle -> c:
    print "$(c.radius)"
case .triangle -> t:
    print "$(t.base) $(t.height)"
case .line -> len:
    print "$(len)"
case .point:
    print "a point"
else:
    print "Unsupported."
```

### Access choice.
A choice can be accessed by specifying the access operator `.!` before the tagged member name. This will either return the payload or panic at runtime: *Planned Feature*
```cy
var s = Shape{line=20}
print s.!line     --> 20
```

## Type aliases.
A type alias refers to a different target type.
Once declared, the alias and the target type can be used interchangeably.

A type alias declaration is denoted as `type A -> T` where `A` is the alias type of `T`:
```cy
type Vec2:
    x float
    y float

type Pos2 -> Vec2

var pos = Pos2{x=3, y=4}
```

## Distinct types.
A distinct type creates a new type by copying a target type.

It's declared with `type` name declaration followed by the target type specifier:
```cy
type Vec2:
    x float
    y float

type Pos2 Vec2

var pos = Pos2{x=3, y=4}
```

Functions can be declared under the new type's namespace:
```
use math

type Pos2 Vec2:
    func blockDist(self, o Pos2):
        var dx = math.abs(o.x - x)
        var dy = math.abs(o.y - y)
        return dx + dy

var pos = Pos2{x=3, y=4}
var dst = Pos2{x=4, y=5}
print pos.blockDist(dst)     --> 2
```
Note that functions declared from the target type do not carry over to the new type.

Unlike a type alias, the new type and the target type can not be used interchangeably since they are different types. However, instances of the new type can be casted to the target type, and vice versa: *Planned Feature*
```cy
type Pos2 Vec2

var a = Pos2{x=3, y=4}

var b Vec2 = a as Vec2
```

## Traits.
A trait type defines a common interface for implementing types. A trait type is declared with the `trait` keyword:
```cy
type Shape trait:
    func area(self) float
```

Types can implement a trait using the `with` keyword:
```cy
type Circle:
    with Shape
    radius float

    func area(self) float:
        return 3.14 * self.radius^2

type Rectangle:
    with Shape
    width  float
    height float

    func area(self) float:
        return self.width * self.height
```
A type that intends to implement a trait but does not satisfy the trait's interface results in a compile error.

Implementing types become assignable to the trait type:
```cy
var s Shape = Circle{radius=2}
print s.area()         --> 12.57

s = Rectangle{width=4, height=5}
print s.area()         --> 20
```

## Type templates.
Type declarations can include template parameters to create a type template:
```cy
type MyContainer[T type]:
    id    int
    value T

    func get(self) T:
        return self.value
```

### Expand type template.
When the type template is invoked with template argument(s), a special version of the type is generated.

In this example, `String` can be used as an argument since it satisfies the `type` parameter constraint:
```cy
var a MyContainer[String] = .{id=123, value='abc'}
print a.get()      -- Prints 'abc'
```
Note that invoking the template again with the same argument(s) returns the same generated type. In other words, the generated type is always memoized from the input parameters.

# C Types.

* [C primitives.](#c-primitives)
* [C structs.](#c-structs)
  * [C struct methods.](#c-struct-methods)
  * [C struct reference.](#c-struct-reference)
* [Pointers.](#pointers)
  * [Dereferencing pointers.](#dereferencing-pointers)
  * [Pointer indexing.](#pointer-indexing)
  * [Pointer arithmetic.](#pointer-arithmetic)
  * [Pointer casting.](#pointer-casting)
  * [Pointer slicing.](#pointer-slicing)
  * [Pointer conversions.](#pointer-conversions)
* [Pointer slices.](#pointer-slices)
* [Union types.](#union-types)

[^top](#table-of-contents)

C types interop with C-ABI types and are used to represent external or manual memory.
Using C types is considered unsafe, but runtime safety checks can be inserted to make it safer at the cost of runtime performance.

## C primitives.
This table shows the size and type compatibility of Cyber and C types:

| Cyber | C | Size (bits) |
| --- | --- | --- |
| void | void | 0, used as *void |
| int8 | int8_t | 8 | 
| byte | uint8_t | 8 |
| c_char | char | 8 |
| int16 | int16_t | 16 | 
| c_short | short | 16* |
| c_uint16 | uint16_t | 16 | 
| c_ushort | unsigned short | 16* |
| int32 | int32_t | 32 | 
| c_int | int | 32* | 
| c_uint32 | uint32_t | 32 | 
| c_uint | unsigned int | 32* |
| int, int64 | int64_t | 64 |
| c_long | long | 64* |
| c_uint64 | uint64_t | 64 |
| c_ulong | unsigned long | 64* |
| c_longlong | long long | 64* |
| c_ulonglong | unsigned long long | 64* |
| float32 | float | 32 |
| float, float64 | double | 64 |
| c_longdouble | long double | 80* |
| *T, pointer[T] | T* | 64* |

\* Varies depending on C compiler and architecture. Pointers may occupy an extra word size for runtime memory checks.

## C structs.
`C structs` are unsafe since they can be referenced by unsafe [pointers](#pointers). The underlying memory could be corrupted or invalidated before use.
In contrast, [structs](#structs) can be safely used since they can only be instantiated in safe memory.

`C structs` can be declared with the `cstruct` keyword:
```cy
type Data cstruct:
    x   float
    y   float
    ptr *int
    str [*]byte
```
A `C struct` may contain:
* C types.
* Primitive types.
* Container types that contain a compatible element type. This includes `enums`, `choices`, and `optionals`.

It may not contain `structs` or `objects`.

### C struct methods.
`C structs` can be declared with methods:
```cy
type Vec2 cstruct:
    x float
    y float

    func add(self, o Vec2):
        return Vec2{
            x = self.x+o.x,
            y = self.y+o.y,
        }

var v = Vec2{x=1, y=2}
print v.add(Vec2{x=2, y=1})   --> Vec2{x=3, y=3}
```
In this example `add` passes the receiver by value.

In order to pass the receiver by reference, `self` must be annotated with `*`:
```cy
type Vec2 cstruct:
    x float
    y float

    func add(*self, o Vec2):
        self.x += o.x
        self.y += o.y

var v = Vec2{x=1, y=2}
v.add(Vec2{x=2, y=1})
print v                 --> Vec2{x=3, y=3}
```

### C struct reference.
The `*` operator is used to obtain the pointer to a `C struct` value:
```cy
type Vec2 cstruct:
    x float
    y float

var v = Vec2{x=1, y=2}
var ref = *v
ref.x = 4

print v                 --> Vec2{x=4, y=2}
```
The reference is a [pointer](#pointers) type that points to the `C struct`:
```
func scale(a *Vec2, n float):
    a.x *= n
    a.y *= n

add(*v, 10)
print v                 --> Vec2{x=40, y=20}
```

## Pointers.
A `pointer` is a reference to a memory location. Its type is denoted as `*T` where `T` is the type that the pointer references in memory:
```cy
func setName(p *Person, name [*]byte):
    p.name = name

var p = Person{}
setName(*p, 'Spock')
```

Depending on the target architecture, the alignment of the pointer will either be at least 8 bytes on a 64-bit machine or 4 bytes on a 32-bit machine. Aligned pointers will be supported in a future version.

Pointers are unsafe since they can reference corrupted or invalidated memory.
When runtime memory checks are enabled, pointers will occupy an extra word size in order to set traps and prevent unsafe uses of pointers. See [Memory / Runtime memory checks](#runtime-memory-checks).

A pointer can be created with an explicit address using `pointer`.
```cy
var ptr = pointer(void, 0xDEADBEEF)
print ptr.value()     --'3735928559'
```

### Dereferencing pointers.
Pointers are dereferenced using the accessor operator `.*`:
```cy
var a = 123
var ptr = *a
print ptr.*      --> 123

ptr.* = 10
print a          --> 10
```

### Pointer indexing.
The index operator is used to read or write to a particular element:
```cy
var a = mem.alloc(int, 10)
a[5] = 123
print a[5]       --> 123
```
Negative indexing will locate the element before the pointer's address.

### Pointer arithmetic.
> _Planned Feature_

### Pointer casting.
> _Planned Feature_

### Pointer slicing.
The slice operator will produce a new slice of type `[*]T` from a `*T` pointer type:
```cy
var ints = mem.alloc(int, 10)
var slice = ints[0..5]
print typeof(slice)    --> []int
```

### Pointer conversions.
> _Planned Feature_

## Pointer slices.
Slices are pointers with a length field. They are denoted as `[*]T` where T is the element type.

A slice can be created by taking the pointer of an array: *Planned feature*
```cy
var arr = [3]int{1, 2, 3}
var s = *arr
```

Read and write an element with the index operator: 
```cy
print s[0]        --> 1
s[1] = 123
print s[1]        --> 123
```

Slices provide bounds checking when runtime safety is enabled: *Planned feature*
```cy
print s[100]      --> Panic. Out of bounds.
```

Slice operations can be applied: *Planned feature*
```cy
print s[0..2]     --> {1, 2}
```

## Union types.
> _Planned Feature_

# Control Flow.
<table><tr>
<td valign="top">

* [Branching.](#branching)
  * [`if` statement.](#if-statement)
  * [`if` expression.](#if-expression)
  * [and/or.](#andor)
* [Iterations.](#iterations)
  * [Infinite `while`.](#infinite-while)
  * [Conditional `while`.](#conditional-while)
  * [`for` range.](#for-range)
  * [`for` each.](#for-each)
  * [`for` each with index.](#for-each-with-index)
  * [Exit loop.](#exit-loop)
  * [Next iteration.](#next-iteration)
</td><td valign="top">

* [`switch` matching.](#switch-matching)
  * [`switch` assignment.](#switch-assignment)
  * [`switch` break.](#switch-break)
* [Try/Catch.](#trycatch)
* [Deferred Execution.](#deferred-execution)
</td>
</tr></table>

[^top](#table-of-contents)

Cyber provides the common constructs to branch and enter loops.

## Branching.

### `if` statement.
Use `if` and `else` statements to branch the execution of your code depending on conditions. The `else` clause can contain a condition which is only evaluated if the previous if or conditional else clause was `false`. 
```cy
var a = 10
if a == 10:
    print 'a is 10'
else a == 20:
    print 'a is 20'
else:
    print 'neither 10 nor 20'
```

### `if` expression.
An `if` expression evaluates a condition in parentheses and returns either the true value or false value.
Unlike the `if` statement, the `if` expression can not contain `else` conditional cases:
```cy
var a = 10
var str = if (a == 10) 'red' else 'blue'
```

### `and`/`or`
Use `and` and `or` logical operators to combine conditions:
```cy
var a = 10
if a > 5 and a < 15:
    print 'a is between 5 and 15'
if a == 20 or a == 10: 
    print 'a is 10 or 20'
```

## Iterations.

### Infinite `while`.
The `while` keyword starts an infinite loop which continues to run the code in the block until a `break` or `return` is reached.
```cy
var count = 0
while:
    if count > 100:
        break
    count += 1
```

### Conditional `while`.
When the `while` clause contains a condition, the loop continues to run until the condition is evaluated to `false`:
```cy
var running = true
var count = 0
while running:
    if count > 100:
        running = false
    count += 1
```

### `for` range.
`for` loops can iterate over a range that starts at an `int` (inclusive) to a target `int` (exclusive).
The capture operator `->` is used to capture the loop's counter variable:
```cy
for 0..4:
    performAction() 

for 0..100 -> i:
    print i    -- 0, 1, 2, ... , 99
```

To decrement the counter instead, use `-..`:
```cy
for 100-..0 -> i:
    print i    -- 100, 99, 98, ... , 1
```

When the range operator `..` is replaced with `..=`, the target `int` is inclusive: *Planned Feature*
```cy
for 0..=100 -> i:
    print i    -- 0, 1, 2, ... , 100
```

### `for` each.
The `for` clause can iterate over any type that implements the `Iterable` trait. An Iterable contains an `iterator()` method which returns an `Iterator` object. The for loop continually invokes the Iterator's `next()` method until `none` is returned.

Lists can be iterated since they implement the Iterable trait. The capture operator `->` is used to capture the value returned from an iterator's `next()`:
```cy
var list = {1, 2, 3, 4, 5}

for list -> n:
    print n
```

Maps can be iterated. `next()` returns a key and value tuple:
```cy
var map = Map{a=123, b=234}

for map -> entry:
    print entry[0]
    print entry[1]
```

Use the destructure syntax to extract the key and value into two separate variables:
```cy
for map -> {key, val}:
    print "key $(key) -> value $(val)"
```

### `for` each with index.
A counting index can be declared after the each variable. The count starts at 0 for the first value:
```cy
var list = {1, 2, 3, 4, 5}

for list -> val, i:
    print "index $(i), value $(val)"
```

### Exit loop.
Use `break` to exit a loop. This loop stops printing once `i` reaches 4:
```cy
for 0..10 -> i:
    if i == 4:
        break
    print i
```

### Next iteration.
Use `continue` to skip the rest of the loop and go to the next iteration.
This loop prints 0 through 9 but skips 4:
```cy
for 0..10 -> i:
    if i == 4:
        continue
    print i
```

## `switch` matching.
The `switch` statement branches to a case block from a matching case condition. The expression that is matched against comes after switch statement. Multiple cases can be grouped together using a comma separator. An optional `else` fallback case is executed when no other cases were matched.
*Incomplete: Not all types can be used in the case conditions such as ranges.*
```cy
var val = 1000
switch val
case 0..100:
    print 'at or between 0 and 99'
case 100:
    print 'val is 100'
case 200:
    print 'val is 200'
case 300, 400:
    print 'combined case'
else:
    print "val is $(val)"
```
The switch statement requires at least one `case` block or an `else` block.
When the switch statement begins a new block, the case statements must be indented:
```cy
switch val:
    case 0: print 'a'
    case 1: print 'b'
    else: print 'c'
```

### `switch` assignment.
Although `switch` can not be used freely as an expression, it can be assigned to a left variable or destination:
```cy
var shu = switch pepper:
    case 'bell'     => 0
    case 'anaheim'  => 500
    case 'jalapeño' => 2000
    case 'serrano'  => 10000
```
When declaring an assignable switch, the cases must have a return value using the syntax `case {cond} => {expr}` or `else => {expr}`.

### `switch` break.
A `break` statement exits the current case block and resumes execution after the end of the switch statement: *Planned Feature*
```cy
switch value
case 0..5:
    print value
    if value == 3:
        break case
    print value    -- Skips second print if `value` is 3.
```

## Try/Catch.
The `try catch` statement, `try else` and `try` expressions provide a way to catch a throwing error and resume execution in a different branch. Learn more about [Error Handling](#error-handling).

## Deferred Execution.
> _Planned Feature_

# Functions.

* [Static functions.](#static-functions)
* [Function overloading.](#function-overloading)
* [Lambdas.](#lambdas)
* [Closures.](#closures)
* [Function types.](#function-types)
* [`extern` functions.](#extern-functions)
* [Multiple parameters.](#multiple-parameters)
* [Named parameters.](#named-parameters)
* [Variadic parameters.](#variadic-parameters)
* [Function calls.](#function-calls)
  * [Shorthand syntax.](#shorthand-syntax)
  * [Call block syntax.](#call-block-syntax)
  * [Piping.](#piping)
* [Function templates.](#function-templates)
  * [Explicit template call.](#explicit-template-call)
  * [Expand function.](#expand-function)
  * [Infer param type.](#infer-param-type)

[^top](#table-of-contents)

In Cyber, there are first-class functions (or function values) and static functions.

## Static functions.
Functions are declared with the `func` keyword and must have a name.
```cy
use math

func dist(x0 float, y0 float, x1 float, y1 float) float:
    var dx = x0-x1
    var dy = y0-y1
    return math.sqrt(dx^2 + dy^2)
```

Functions can be invoked with arguments:
```cy
print dist(0, 0, 10, 20)
```

Functions are initially static, but they can be passed around as a lambda or assigned to a variable:
```cy
-- Assigning to a local variable.
var bar = dist

func squareDist(dist dyn, size float) float:
    return dist(0.0, 0.0, size, size)
    
-- Passing `dist` as an argument.
print squareDist(dist, 30.0)
```

Functions can only return one value. However, the value can be destructured: *Planned Feature*
```cy
use {cos, sin} 'math'

func compute(rad float) [2]float:
    return .{cos(rad), sin(rad)}

var {x, y} = compute(pi)
```

## Function overloading.
Functions can be overloaded by their type signature:
```cy
func foo() int:
    return 2 + 2

func foo(n int) int:
    return 10 + n

func foo(n int, m int) int:
    return n * m

print foo()         --> 4
print foo(2)        --> 12
print foo(20, 5)    --> 100
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
var add = func (a int, b int) int:
    return a + b

-- Passing a lambda block as an argument.
canvas.onUpdate():
    ..func (delta_ms):
        print delta_ms
```
Passing a lambda block as a call argument is only possible in a call block. *Planned Feature* See [Function calls](#function-calls).

## Closures.
Lambdas can capture local variables from parent blocks. This example shows the lambda `f` capturing `a` from the main scope: *Incomplete, only variables one parent block away can be captured.*
```cy
var a = 1
var f = func() int:
    return a + 2
print f()         --> 3
```

The following lambda expression captures `a` from the function `add`:
```cy
func add():
    var a = 123
    return b => a + b
var addTo = add()
print addTo(10)   --> 133
```

Like static variables, static functions can not reference local variables outside of their scope:
```cy
var a = 1
func foo():
    print a       --> CompileError: Undeclared variable `a`.
```

## `extern` functions.
> _Planned Feature_

## Function types.
A function type is denoted as `func(P1, P2, ...) R` where `P`s are parameter types and `R` is the return type in addition to any function modifiers:
```cy
type AddFn -> func(int, int) int
```

Function types can include optional parameter names:
```cy
type AddFn -> func(a int, b int) int
```
If one parameter has a name, the other parameters must also have names.
Parameter names do not alter the function signature and only serve to document the function type.

Only static functions can be assigned to a function type:
```cy
func add(a int, b int) int:
    return a + b

var fn AddFn = add
fn(10, 20)         --> 30
```

Similarily, a function union type is denoted as `Func(P1, P2, ...) R` and can store static functions, lambdas, and closures:
```cy
var c = 5
func addClosure(a int, b int) int:
    return a + b + c

var fn Func(int, int) int = add
fn(10, 20)         --> 30
fn = addClosure 
fn(10, 20)         --> 35
```

## Multiple parameters.
> _Planned Feature_

When multiple parameters share the same type they can be declared together in a sequence:
```cy
func sum(a, b, c int) int
    return a + b + c
```

## Named parameters.
> _Planned Feature_

## Variadic parameters.
> _Planned Feature_

## Function calls.
The straightforward way to call a function is to use parentheses.

```cy
var d = dist(100, 100, 200, 200)
```

You can call functions with named parameters.
> _Planned Feature_
```cy
var d = dist(x0=10, x1=20, y0=30, y1=40)
```

### Shorthand syntax.
Functions can be invoked without parentheses. This only works for calls that take at least one argument. Named arguments are not allowed and nested function calls must use parentheses:
```cy
-- Calls the function `dist`.
var d = dist 100, 100, 200, 200

func foo():       
    return 4

var r = foo       -- Returns the function itself as a value.
                     -- Does not call the function `random`.
r = foo()         -- Calls the function `random`.
```

### Call block syntax.
The call block appends a lambda to a call expression's arguments:
```cy
func Button(name String, size int, on_click any):
    return {
        name = name,
        size = size,
        on_click = on_click,
    }

Button('Count', 10):
    print 'on click'
```

If arguments are omitted from the call expression they can be appended inside the call block using named arguments: *Planned Feature*
```cy
Button('Count'):
    .size = 10
    print 'on click'
```
Arguments assigned in the call block can be unordered.

### Piping.
> _Planned Feature_

## Function templates.
Function declarations can include template parameters to create a function template:
```cy
func add(#T type, a T, b T) T:
    return a + b
```
Template parameters are prefixed with `#`.
Unlike type templates, function templates allow template parameters to be declared alongside runtime parameters.

### Explicit template call.
When the function is invoked with template argument(s), a special version of the function is generated and used:
```cy
print add(int, 1, 2)    --> 3
print add(float, 1, 2)  --> 3.0
```
Note that invoking the function again with the same argument(s) uses the same generated function. In other words, the generated function is always memoized from the template arguments.

### Expand function.
Since functions may contain template and runtime parameters, the index operator is used to expand the function with just template arguments and return the generated function:
```cy
var addInt = add[int]
print addInt(1, 2)      --> 3
```

### Infer param type.
When a template parameter is declared in the type specifier, it's inferred from the argument's type:
```cy
func add(a #T, b T) T:
    return a + b

print add(1, 2)         --> 3
print add(1.0, 2.0)     --> 3.0
```
In the above example, `add[int]` and `add[float]` were inferred from the function calls:

Nested template parameters can also be inferred:
```cy
func set(m Map[#K, #V], key K, val V):
    m.set(key, val)
```

# Modules.
<table><tr>
<td valign="top">

* [Importing.](#importing)
  * [Import file.](#import-file)
  * [Import URL.](#import-url)
  * [Import all.](#import-all)
  * [Main module.](#main-module)
  * [Circular imports.](#circular-imports)
  * [Destructure import.](#destructure-import)
* [Exporting.](#exporting)
* [Module URI.](#module-uri)
* [Symbol visibility.](#symbol-visibility)
* [Symbol alias.](#symbol-alias)
* [Builtin modules.](#builtin-modules)
* [`mod core`](#mod-core)
  * [`type bool`](#type-bool)
  * [`type error`](#type-error)
  * [`type int`](#type-int)
  * [`type float`](#type-float)
  * [`type List`](#type-list)
  * [`type ListIterator`](#type-listiterator)
  * [`type Tuple`](#type-tuple)
  * [`type Table`](#type-table)
  * [`type Map`](#type-map)
  * [`type MapIterator`](#type-mapiterator)
  * [`type String`](#type-string)
  * [`type Array`](#type-array)
  * [`type ArrayIterator`](#type-arrayiterator)
  * [`type pointer`](#type-pointer)
  * [`type Fiber`](#type-fiber)
  * [`type metatype`](#type-metatype)

</td><td valign="top">

* [`mod cy`](#mod-cy)
* [`mod math`](#mod-math)
* [Std modules.](#std-modules)
* [`mod cli`](#mod-cli)
* [`mod os`](#mod-os)
  * [`type File`](#type-file)
  * [`type Dir`](#type-dir)
  * [`type DirIterator`](#type-diriterator)
  * [`type FFI`](#type-ffi)
  * [`type CArray`](#type-carray)
  * [`type CDimArray`](#type-cdimarray)
  * [`Map DirEntry`](#map-direntry)
  * [`Map DirWalkEntry`](#map-dirwalkentry)
  * [`Table ArgOption`](#table-argoption)
* [`mod test`](#mod-test)
</td>
</tr></table>

[^top](#table-of-contents)

Modules have their own namespace and contain accessible static symbols. By default, importing another Cyber script returns a module with its declared symbols.

## Importing.
When a `use` declaration contains only a single identifier, it creates a local alias to a module using the identifier as the module specifier. Cyber's CLI comes with some builtin modules like `math` and `test`. 
```cy
use test
test.eq(123, 123)

use math
print math.cos(0)
```

The explicit import syntax requires an alias name followed by a module specifier as a raw string:
```cy
use m 'math'
print m.random()
```
When Cyber is embedded into a host application, the module resolver and loader can be overridden using [libcyber](#libcyber).

### Import file.
File modules can be imported:

```cy
-- Importing a module from the local directory.
use b 'bar.cy'
print b.myFunc()
print b.myVar
```

### Import URL.
Modules can be imported over the Internet:
```cy
-- Importing a module from a CDN.
use rl 'https://mycdn.com/raylib'
```
When importing using a URL without a file name, Cyber's CLI will look for a `mod.cy` from the path instead.

### Import all.
If the alias name is the wildcard character, all symbols from the module are imported into the using namespace: *This feature is experimental and may be removed in a future version.*
```cy
use * 'math'
print random()
```

### Main module.
Only the main module can have top-level statements that aren't static declarations.
An imported module containing top-level statements returns an error:
```cy
-- main.cy
use a 'foo.cy'
print a.foo

-- foo.cy
use 'bar.cy'
var .foo = 123
print foo         -- Error: Top-level statement not allowed.
```

### Circular imports.
Circular imports are allowed. In the following example, `main.cy` and `foo.cy` import each other without any problems.
```cy
-- main.cy
use foo 'foo.cy'

func printB():
    foo.printC()

foo.printA()

-- foo.cy
use main 'main.cy'

func printA():
    main.printB()

func printC():
    print 'done'
```
Static variable declarations from imports can have circular references. Read more about this in [Static variables](#static-variables).

### Destructure import.
Modules can also be destructured using the following syntax:
> _Planned Feature_
```cy
use { cos, pi } 'math'
print cos(pi)
```

## Exporting.
All symbols are exported when the script's module is loaded. However, symbols can be declared with a [hidden](#symbol-visibility) modifier.
```cy
func foo():         -- Exported static function.
    print 123

var .bar = 234      -- Exported static variable.

type Thing:  -- Exported type.
    var a float
```

## Module URI.
To get the absolute path of the current module, reference the compile-time constant `#modUri`.
This can be used with `os.dirName` to get the current module directory.
```cy
print #modUri              -- Prints '/some/path/foo.cy'

use os
print os.dirName(#modUri)  -- Prints '/some/path'
```

## Symbol visibility.
All symbols have public visibility. However, when a symbol is declared with a `-` prefix, it suggests that it should not be made available to an editor's autocomplete:
```cy
-type Foo:
    a int
    b int

-func add(a int, b int) int:
    return a + b
```
Furthermore, the symbol is excluded when its module is included using `use *`.

## Symbol alias.
`use` can be used to create an alias to another symbol:
```cy
use eng 'lib/engine.cy'
use Vec2 -> eng.Vector2
```

## Builtin modules.
Builtin modules are the bare minimum that comes with Cyber. The [embeddable library](#embedding) contains these modules and nothing more. They include:
- [core](#mod-core): Commonly used utilities.
- [cy](#mod-cy): Cyber related functions.
- [math](#mod-math): Math constants and functions.

## `mod core`
The `core` module contains functions related to Cyber and common utilities. It is automatically imported into each script's namespace. 

Sample usage:
```cy
-- `print` and `typeof` are available without imports.
print 'hello'
print typeof('my str').id()
```

[^topic](#modules)

<!-- core.start -->
<!-- core.end -->

## `mod cy`
The `cy` module contains functions related to the Cyber language. 

Sample usage:
```cy
use cy
print cy.toCyon([1, 2, 3])
```

[^topic](#modules)

<!-- cy.start -->
<!-- cy.end -->

## `mod math`
The math module contains commonly used math constants and functions.

Sample usage:
```cy
use math

var r = 10.0
print(math.pi * r^2)
```

[^topic](#modules)

<!-- math.start -->
<!-- math.end -->

## Std modules.
Std modules come with Cyber's CLI. They include:
- [cli](#mod-cli): Related to the command line.
- [os](#mod-os): System level functions.
- [test](#mod-test): Utilities for testing.

## `mod cli`
The `cli` module contains functions related to the command line.

Sample usage:
```cy
use cli
cli.repl()
```

[^topic](#modules)

<!-- cli.start -->
<!-- cli.end -->

## `mod os`
Cyber's os module contains system level functions. It's still undecided as to how much should be included here so it's incomplete. You can still access os and libc functions yourself using Cyber's FFI or embedding API.

Sample usage:
```cy
use os

var map = os.getEnvAll()
for map -> {k, v}:
    print "$(k) -> $(v)"
```

[^topic](#modules)

<!-- os.start -->
<!-- os.end -->
### `Map DirEntry`
| key | summary |
| -- | -- |
| `'name' -> Array` | The name of the file or directory. |
| `'type' -> #file | #dir | #unknown` | The type of the entry. |

### `Map DirWalkEntry`
| key | summary |
| -- | -- |
| `'name' -> Array` | The name of the file or directory. |
| `'path' -> Array` | The path of the file or directory relative to the walker's root directory. |
| `'type' -> #file | #dir | #unknown` | The type of the entry. |

### `Table ArgOption`
| key | summary |
| -- | -- |
| `'name' -> String` | The name of the option to match excluding the hyphen prefix. eg. `-path` |
| `'type' -> metatype(String | float | boolean)` | Parse as given value type. |
| `'default' -> any` | Optional: Default value if option is missing. `none` is used if this is not provided. |

## `mod test`
The `test` module contains utilities for testing.

Sample usage:
```cy
use t 'test'

var a = 123 + 321
t.eq(a, 444)
```

[^topic](#modules)

<!-- test.start -->
<!-- test.end -->

# FFI.

* [FFI context.](#ffi-context)
* [Declare functions.](#declare-functions)
* [Bind library.](#bind-library)
  * [Search path.](#search-path)
  * [Configuration.](#configuration)
  * [Finalizer.](#finalizer)
* [Mappings.](#mappings)
* [Bind to Cyber type.](#bind-to-cyber-type)
* [cbindgen.cy](#cbindgency)

[^top](#table-of-contents)

Cyber supports binding to an existing C ABI compatible library at runtime.
This allows you to call into dynamic libraries created in C or other languages.
Cyber uses `libtcc` to JIT compile the bindings so function calls are fast.
The example shown below can be found in [Examples](https://github.com/fubark/cyber/blob/master/examples/ffi.cy).

## FFI context.
An FFI context contains declarations that map C to Cyber. Afterwards, it allows you to bind to a dynamic library or create interoperable objects. To create a new `FFI` context:
```cy
use os

var ffi = os.newFFI()
```

## Declare functions.
Functions from a library are first declared using `cfunc` which accepts C types in the form of symbols. In a future update they will accept C syntax instead.
```cy
ffi.cfunc('add', {.int, .int}, .int)
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
let lib = ffi.bindLib('./mylib.so')
lib.add(123, 321)
```
Note that `let` is used to allow `lib` to be used dynamically since the type is unknown at compile-time.

### Search path.
If the path argument to `bindLib` is just a filename, the search steps for the library is specific to the operating system. Provide an absolute (eg. '/foo/mylib.so') or relative (eg. './mylib.so') path to load from a direct location instead. When the path argument is `none`, it loads the currently running executable as a library allowing you to bind exported functions from the Cyber CLI or your own application/runtime.

### Configuration.
By default `bindLib` returns an anonymous object with the binded C-functions as methods. This is convenient for invoking functions using the method call syntax. If a config is passed into `bindLib` as the second argument, `gen_table=true` makes `bindLib` return a table instead with the binded C-functions as Cyber functions.

### Finalizer.
The resulting object of `bindLib` holds a reference to an internal TCCState which owns the loaded JIT code.
Once the object is released by ARC, the TCCState is also released which removes the JIT code from memory.

## Mappings.
When using `cfunc` or `cbind` declarations, [symbols](#symbols) are used to represent default type mappings from Cyber to C and back:
> _Incomplete: This is not the final API for dynamically loading and interfacing with C libraries. The plan is to parse a subset of C headers to bind to Cyber types and functions._

| Binding | Cyber | C |
| --- | --- | --- |
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
| (1) .charPtr | *void | char* |
| .voidPtr | *void | void* |
| (2) type {S} | type {S} | struct |

1. Use `os.cstr()` and `pointer.fromCstr()` to convert between a Cyber string and a null terminated C string.
2. The mapping from a Cyber object type `S` and the C-struct can be declared with `cbind`.

## Bind to Cyber type.
`cbind` is used to bind a C struct to a Cyber object type. Once declared, the Cyber type can be used as a binding type in function declarations:
```cy
use os

type MyObject:
    a float
    b *void
    c bool

ffi.cbind(MyObject, {.float, .voidPtr, .bool})
ffi.cfunc('foo', {MyObject}, MyObject)
let lib = ffi.bindLib('./mylib.so')

var res = lib.foo(MyObject{a=123.0, b=os.cstr('foo'), c=true})
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
ffi.cfunc('foo', {MyObject}, .voidPtr)
let lib = ffi.bindLib('./mylib.so')

var ptr = lib.foo(MyObject{a=123, b=os.cstr('foo'), c=true})
var res = lib.ptrToMyObject(ptr)
```

## cbindgen.cy
[cbindgen.cy](https://github.com/fubark/cyber/blob/master/src/tools/cbindgen.cy) is a Cyber script that automatically generates bindings given a C header file. Some example bindings that were generated include: [Raylib](https://github.com/fubark/ray-cyber) and [LLVM](https://github.com/fubark/cyber/blob/master/src/tools/llvm.cy).

# Error Handling.
<table><tr>
<td valign="top">

* [Error value.](#error-value)
  * [`error` literal.](#error-literal)
  * [`error` payload.](#error-payload)
  * [`error` set type.](#error-set-type)
* [Throwing errors.](#throwing-errors)
* [Catching errors.](#catching-errors)
  * [`try` block.](#try-block)
  * [`caught` variable.](#caught-variable)
  * [`catch` matching.](#catch-matching)
  * [`try` expression.](#try-expression)
  * [Value or error.](#value-or-error)
</td><td valign="top">

* [Semantic checks.](#semantic-checks)
  * [Throws modifier.](#throws-modifier)
  * [Throws check.](#throws-check)
* [Stack trace.](#stack-trace)
* [Unexpected errors.](#unexpected-errors)
  * [Panics.](#panics)
</td>
</tr></table>

[^top](#table-of-contents)

Cyber provides a throw/catch mechanism to handle expected errors. For unexpected errors, panics can be used as a fail-fast mechanism to abort the currently running fiber.

## Error value.

### `error` literal.
An `error` value contains a `symbol`. They can be created without a declaration using the error literal:
```cy
var err = error.Oops
```

Use `sym()` to obtain the underlying symbol:
```cy
print err.sym()   -- Prints ".Oops"
```

Since `error` is a primitive value, it can be compared using the `==` operator.
```cy
if err == error.Oops:
    handleOops()

-- Alternatively.
if err.sym() == .Oops:
    handleOops()
```

### `error` payload.
An payload value can be attached when throwing an error value. *Planned Feature*

### `error` set type.
Error set types enumerate error values that belong to the same group of errors: *Planned Feature*
```cy
type MyError error:
    case boom
    case badArgument
    case nameTooLong

var err = MyError.nameTooLong
```

## Throwing errors.
Use the `throw` keyword to throw errors. 
A thrown error continues to bubble up the call stack until it is caught by a `try` block or `try` expression.
```cy
func fail():
    throw error.Oops      -- Throws an error with the symbol `#Oops`

func fail2():
    throw 123             -- panic: Can only throw an `error` value.
```

`throw` can also be used as an expression.
```cy
func fail():
    var a = false or throw error.False
```

## Catching errors.

### `try` block.
The `try` block catches thrown errors and resumes execution in a followup `catch` block:
```cy
try:
    funcThatCanFail()
catch err:
    print err      -- 'error.Failed'
```

### `caught` variable.
The contextual `caught` variable is used to reference the caught error: *Planned Feature*
```cy
try:
    funcThatCanFail()
catch:
    print caught   -- 'error.Failed'
```

### `catch` matching.
An inner `catch` block contains a matching clause: *Planned Feature*
```cy
try:
    funcThatCanFail()
catch error.BadDay:
    eatSnack()
catch:
    print caught
```

Error sets can be matched: *Planned Feature*
```cy
try:
    funcThatCanFail()
catch MyError.Boom:
    print 'Kaboom!'
catch:
    print caught
```

### `try` expression.
The `try` expression either returns a non-error result or the default value from the `catch` clause:
```cy
var res = try funcThatCanFail() catch 123
print res         -- '123'
```

Since errors bubble up automatically, any errors thrown from sub-expressions are also caught:
```cy
var res = try happyFunc(funcThatCanFail()) catch 123
print res         -- '123'
```

### Value or error.
When the `catch` clause is omitted, the `try` expression will return either the value or the error:
```cy
var res = try funcThatCanFail()
if res == error.Failed:
    print 'Result is an error.'
```

## Semantic checks.
### Throws modifier.
The throws modifier `!` indicates that a function contains a throwing expression that was not caught with `try catch`.

The modifier is attached to the function return type as a prefix:
```cy
func foo() !void:
    throw error.Failure
```

This declaration indicates the function can either return an `int` type or throw an error:
```cy
func result(cond bool) !int:
    if cond:
        return 123
    else:
        throw error.Failure
```

### Throws check.
The compiler requires a throws modifier if the function contains an uncaught throwing expression: *Planned Feature*
```cy
func foo(a int) int:
    if a == 10:
        throw error.Failure   --> CompileError.
    else:
        return a * 2

--> CompileError. Uncaught throwing expression.
--> `foo` requires the `!` throws modifier or
--> the expression must be caught with `try`.
```

## Stack trace.
When an uncaught error bubbles up to the top, its stack trace from the `throw` callsite is dumped to the console. The builtin `errorTrace()` and `errorReport()` are used to obtain the stack trace info.
```cy
try:
    funcThatCanFail()
catch:
    -- Prints the stack trace summary of the caught error.
    print errorReport()

    -- Provides structured info about the stack trace.
    var info = errorTrace()
    print info.frames.len()
```

## Unexpected errors.
An unexpected error is an error that is not meant to be handled at runtime.

### Panics.
The builtin `panic` is used as a fail-fast mechanism to quickly exit the current fiber with an error payload:
```cy
func kaboom():
    panic(error.danger)

kaboom()     -- Script ends and prints the stack trace.
```
Panics can not be caught using `try catch`. Once `panic` is invoked, the current fiber stops execution and begins to unwind its call stack. Once the error is propagated to the root, the fiber ends and transitions to a panic state. If the main fiber ends this way, the VM begins to shutdown. Otherwise, execution resumes on the next fiber which allows recovery from a panic.

# Concurrency.

<table><tr>
<td valign="top">

* [Async.](#async)
  * [Futures.](#futures)
  * [`await`.](#await)
  * [Colorless async.](#colorless-async)
  * [Future chains.](#future-chains)
  * [Resolving futures.](#resolving-futures)
  * [Structured concurrency.](#structured-concurrency)
  * [Finish policies.](#finish-policy)
  * [Task cancellation.](#task-cancellation)
  * [Threads.](#threads)
</td><td valign="top">

* [Fibers.](#fibers)
  * [Creating fibers.](#creating-fibers)
  * [Passing arguments.](#passing-arguments)
  * [Reset state.](#reset-state)
  * [Rebinding arguments.](#rebinding-arguments)
  * [Fiber block.](#fiber-block)
  * [Pause and resume.](#pause-and-resume)
  * [Fiber state.](#fiber-state)
* [Gas mileage.](#gas-mileage)
</td>
</tr></table>

[^top](#table-of-contents)

Cyber supports asynchronous and cooperative concurrency.

## Async.
Cyber supports asynchronous execution which provides preemptive concurrency.
Tasks can be scheduled over a single thread. *Multi-threaded execution is planned.*

### Futures.
A `Future` is a promise that some work will either complete or fail at some point in the future.
This abstraction allows the current thread to continue execution without waiting for the completion of the future.

The asynchronous work encapsulated by the future has the opportunity to run in parallel.
For example, I/O bound work can be delegated to the operating system and CPU bound work can be run across multiple threads.
Some work may simply run on the same thread.

If an API function is meant to do work asynchronously, it would return a `Future`:
```cy
use aio

var f = aio.delay(1000)
print f          --> Future(void)
```

Futures can hold a result value when they are completed:
```cy
use aio

var f = aio.readFile('foo.txt')
print f          --> Future(String)
```

Futures can be created with a completed value:
```cy
var f = Future.complete(100)
print f          --> Future(int)
print f.get().?  --> 100
```

Future are also created when composing them together.

### `await`.
The `await` expression asynchronously waits for a `Future` to resolve.
It guarantees that the current execution flow will not resume until the future is completed.
Once resumed, the expression evaluates to the completed result:
```cy
use aio

var res = await aio.readFile('foo.txt')
print res        --> bar
```
In the above example, the file contents of `foo.txt` is "bar".

`await` suspends the current fiber so the scheduler can execute tasks in its ready queue.
When the future resolves, the suspended task is moved to the ready queue waiting to resume execution.

Performing `await` on other values besides a `Future` type will evaluate to the value and will not perform any waiting:
```cy
var v = await 123
print v          --> 123
```

### Colorless async.
`await` can be used in any function.
This means that async functions are colorless and don't require a special function modifier.

### Future chains.
`Future.then` is used to attach a callback that will only be invoked when the future completes, thereby creating an asynchronous chain: *Planned Feature*
```cy
use aio

var f = aio.readFile('foo.txt').then() -> res:
    print res    --> bar

print f          --> Future(void)
```
By default `Future.then` evaluates to a new `Future(void)` and represents the completion of the callback.
A callback that returns a result requires a generic parameter or inferred from the lambda: *Planned Feature*
```cy
var f = aio.readFile('foo.txt').then() -> res:
    return res.len()

print f          --> Future(int)
print await f    --> 3
```

Similarily, `Future.catch` attaches a callback that will only be invoked when the future fails: *Planned Feature*
```cy
var f = aio.readFile('foo.txt').catch() -> err:
    print err    --> error.FileNotFound
```

### Resolving futures. 
`FutureResolver` produces a completable `Future`. In the following example, a future is completed with the `FutureResolver` after a queued task runs:
```cy
var r = FutureResolver[int].new()

queueTask():
    r.complete(234)

var v = await r.future()
print v
```

### Structured concurrency.
> _Planned Feature_

Asychronous tasks must be created with an `Async` context which groups together related tasks.
This gives tasks well-defined lifetimes and allows policies to be applied to a task group.

Every program begins by pushing a new `Async.waitAll` context to the `async` context variable. 
`waitAll` is a [finish policy](#finish-policies) that waits for all tasks to complete before it marks its finish task as completed.
In the following example, the created tasks are attached to the current `Async` context:
```cy
use aio

aio.delay(1000)
addMoreTasks()
```
Since the program did not explicitly wait for the completion of the current `Async` context, execution will end before 1 second has elapsed. 

To wait for all the tasks to complete, the program can wait for the `Async` context to finish:
```cy
use aio

context async Async

aio.delay(1000)
addMoreTasks()

await async.finish()
```
The context variable `async` is declared to access the current `Async` context.
`finish` applies the finish policy `waitAll` which completes when all tasks under the async context have been completed.

After invoking `finish`, new tasks can no longer be created under the async context.
Doing so would result in a runtime error.
A future is returned to indicate the completion of the finish task.

Explicitly invoking `finish` isn't idiomatic and was only demonstrated to show how an async context works. The same feat can be achieved with helpers that can wrap a block of related async logic:
```cy
use aio

await Async.block(.waitAll):
    aio.delay(1000)
    addMoreTasks()
```
`Async.block` begins by creating a new async context and pushing the context to `async`.
The callback is then invoked which creates a new task under the `async` context. 
Once the callback returns, the `async` context and the result of `Async.finish` is returned.

### Finish policies.
> _Planned Feature_

### Task cancellation.
> _Planned Feature_

### Threads.
> _Planned Feature_

## Fibers.
A fiber represents a separate execution context as a first-class value. It contains it's own call stack and program counter. Fibers by themselves do not enable parallelism.

### Creating fibers.
The `coinit` keyword creates and returns a new fiber using a function as the entry point:
```cy
var count = 0

var foo = func ():
    count += 1
    coyield
    count += 1

var task = coinit(foo)

print count          -- '0'
coresume task
print count          -- '1'
coresume task
print count          -- '2'
```
A fiber does not start execution until `coresume` is invoked on it.
`coyield` pauses the current fiber and execution is returned to the previous fiber that invoked `coresume`.

### Passing arguments.
Arguments after the callee are passed into the entry function:
```cy
var count = 0

var increment = func (inc):
    count += inc

var task = coinit(increment, 5)
coresume task
print count          -- '5'
```
When the fiber is created, the arguments are saved inside the fiber's stack. Once the first `coresume` is invoked, the entry function is invoked with the saved arguments.

### Reset state.
To reset a fiber to its initial state, invoke `reset()`. *Planned Feature*
When reset, the existing stack is unwinded, the program counter returns to the starting point, and the state is set to `.init`:
```cy
func fib(n int) int:
    coyield n
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

var task = coinit(fib, 10)

-- Progress the fiber...
print(coresume task)    -- Prints "10"
print(coresume task)    -- Prints "9"
print(coresume task)    -- Prints "8"

-- Reset back to the start with the `.init` state.
task.reset()
print(coresume task)    -- Prints "10"
```

### Rebinding arguments.
Arguments attached to the fiber can be rebinded with a different set of values. *Planned Feature*
This allows fiber reuse, instead of creating a new fiber:
```cy
var task = coinit(fib, 10)

-- Run task to completion.
var res = 0
while task.status() != .done:
    res = coresume fiber
print res

task.reset()
task.bindArgs(20)

-- Run task again with the new argument...
```

### Fiber block.
A fiber block is used to construct a fiber without an entry function. *Planned Feature* The counting example can be rewritten to:
```cy
var count = 0

var task = coinit:
    count += 1       -- `count is captured`
    coyield
    count += 1

print count          -- '0'
coresume task
print count          -- '1'
coresume task
print count          -- '2'
```
Referencing parent variables from the fiber block automatically captures them just like a function closure.

### Pause and resume.
`coyield` can be used anywhere in a fiber's call stack to pause execution and return to the previous fiber.
```cy
func foo():
    print 'foo'
    bar()

func bar():
    -- Nested coyield in call stack.
    coyield
    print 'bar'

var task = coinit(foo)
coresume task
```
`coresume` also returns the resulting value.
```cy
func foo():
    return 123

var task = coinit(foo)
print(coresume task)    -- '123'
```

`coyield` can return a value back to `coresume`. *Planned Feature*

### Fiber state.
Use `Fiber.status()` to get the current state of the fiber.
```cy
func foo():
    coyield
    print 'done'

var task = coinit(foo)
print task.status()   -- '.paused'
coresume task
print task.status()   -- '.paused'
coresume task
print task.status()   -- '.done'
```
The main execution context is a fiber as well. Once the main fiber has finished, the VM is done and control is returned to the host.

## Gas mileage.
> _Planned Feature_

# Dynamic Typing.

* [Dynamic variables.](#dynamic-variables)
* [Runtime type checking.](#runtime-type-checking)
* [Dynamic functions.](#dynamic-functions)
* [Dynamic tables.](#dynamic-tables)
* [Custom tables.](#custom-tables)
* [Dynamic inference.](#dynamic-inference)

[^top](#table-of-contents)

Dynamic typing is supported with a less restrictive syntax. This can reduce the amount of friction when writing code, but it can also result in more runtime errors.

In Cyber, the `let` keyword is used exclusively for dynamic declarations.

## Dynamic variables.
Variables declared with `let` are implicitly given the `dyn` type:
```cy
let a = 123
```

Typically a dynamic variable defers type checking to runtime, but if the compiler determines that an operation will always fail at runtime, a compile error is reported instead:
```cy
let a = '100'

print a / 2
--> CompileError: Can not find the symbol `$infix/` in `String`
```

When `a` is assigned a different type of value, its **recent type** is updated so the compiler can continue to surface errors ahead of time:
```cy
a = {1, 2, 3}
print a / 2
--> CompileError: Can not find the symbol `$infix/` in `List`
```

## Runtime type checking.
If the type of a dynamic variable can not be determined at compile-time, type checking is deferred to runtime.

In this example, the type for `a` is unknown after assigning the return of a dynamic call to `erase`.
Any operation on `a` would defer type checking to runtime:
```cy
let a = erase(123)

print a(1, 2, 3)
--> panic: Expected a function.
```

If a dynamic variable's **recent type** differs between two branches of execution, the type is considered unknown after the branches are merged. Any operations on the variable afterwards will defer type checking to runtime:
```cy
let a = 123
if a > 20:
    a = 'hello'

-- Branches are merged. `a` has an unknown type.

print a(1, 2, 3)
--> panic: Expected a function.
```

## Dynamic functions.
Functions declared with `let` do not allow typed parameters or a return specifier.
All parameters are implicitly given the `dyn` type.

The return specifier is also implicitly `!dyn` which indicates that the function can throw an error. This is only relevant when typed code calls a dynamic function:
```cy
let foo(a, b, c):
    return a + b() + a[c]
```

The function `foo` is a static function. It can't be reassigned like a variable after its declaration.

However, function values (lambdas) assigned to variables allow reassignment. Lambdas can also capture variables from the parent scope:
```cy
let count = 123
let f = (a, b):
    return a + b(10) + count

-- Reassign `f`.
f = (a, b):
    return count * 2 - b(a)
```

Lambdas that simply return an expression can be written as:
```cy
f = (a, b) => a + b(10)
```

## Dynamic tables.
The builtin `Table` type is used to create dynamic objects.
Tables are initialized with the record literal:
```cy
let a = {}
a.name = 'Nova'
print a.name     --> Nova
```
Read more about how to use [Tables](#tables).

## Custom tables.
Custom tables allow declaring fields and methods.

```cy
let Counter{ count }:
    let inc():
        count += 1
        
let c = Counter{count=0}
c.inc()
print c.count       --> 1
```
Fields are declared inside the braces and separated with commas.
Unlike typed declarations, the fields declared only serve as a subset constraint.
Additional fields can still be initialized and used.

Tables can be declared without any methods:
```cy
let Vec3{x, y, z}

let v = Vec3{x=1, y=2, z=3}
```

## Dynamic inference.
When the inference tag is used in a dynamic context, it will attempt to resolve its value at runtime.
In this example, the dynamic value `a` resolves to a `String` at runtime and invokes the typed method `trim`.
`.left` then infers to the correct value at runtime:
```cy
print str.trim(.left, ' ')
```

# Metaprogramming.

<table><tr>
<td valign="top">

* [Operator overloading.](#operator-overloading-1)
  * [Builtin operators.](#builtin-operators)
  * [Custom operators.](#custom-operators)
* [Magic functions.](#magic-functions)
  * [Call module.](#call-module)
  * [`$initRecord` method.](#initrecord-method)
  * [`$initPair` method.](#initpair-method)
  * [`$get` method.](#get-method)
  * [`$set` method.](#set-method)
  * [Missing method.](#missing-method)
</td><td valign="top">

* [Reflection.](#reflection)
* [Attributes.](#attributes)
* [Templates.](#templates)
  * [Template specialization.](#template-specialization)
* [Macros.](#macros)
* [Compile-time execution.](#compile-time-execution)
  * [Builtin types.](#builtin-types)
  * [Builtin functions.](#builtin-functions)
  * [Builtin constants.](#builtin-constants)
* [Runtime execution.](#runtime-execution)
</td>
</tr></table>

[^top](#table-of-contents)

## Operator overloading.
All operators are implemented as object methods.
> _Incomplete: Not all operators have transitioned to the method paradigm._ 

To overload an operator for an object type, declare `$prefix`, `$infix`, `$postfix` methods. See the available [builtin operators](#builtin-operators). Since operator names aren't allowed as standard identifiers, they are contained in a string literal.
```cy
type Vec2:
    x float
    y float

    func '$infix+'(self, o Vec2) Vec2:
        return Vec2{
            x = x + o.x,
            y = y + o.y,
        }

    func '$prefix-'(self) Vec2:
        return Vec2{x=-x, y=-y}

var a = Vec2{x=1, y=2}
var b = a + Vec2{x=3, y=4}
var c = -a
```

Some special operators have their own name. This example overloads the `index` operator and the `set index` operator:
```cy
type MyCollection:
    arr List

    func $index(self, idx):
        return arr[idx * 2]

    func $setIndex(self, idx, val):
        arr[idx * 2] = val 

var a = MyCollection{arr={1, 2, 3, 4}}
print a[1]        -- Prints `3`
```

### Builtin operators.
A list of all supported operators:

| Operator | Name |
| --- | --- |
| Bitwise not | `$prefix~` |
| Minus | `$prefix-` |
| Greater | `$infix>` |
| Greater equal | `$infix>=` |
| Less | `$infix<` |
| Less equal | `$infix<=` |
| Add | `$infix+` |
| Subtract | `$infix-` |
| Multiply | `$infix*` |
| Divide | `$infix/` |
| Modulus | `$infix%` |
| Power | `$infix^` |
| Bitwise and | `$infix&` |
| Bitwise or | `$infix|` |
| Bitwise xor | `$infix||` |
| Bitwise left shift | `$infix<<` |
| Bitwise right shift | `$infix>>` |
| Index | `$index` |
| Set index | `$setIndex` |
| Slice | `$slice` |

### Custom operators.
> _Planned Feature_

## Magic functions.

### Call module.
Declare a `$call` function to allow invoking a module as a function.
```cy
-- Object types are also modules.
type Vec2:
    x float
    y float

func Vec2.$call(x float, y float) Vec2:
    return Vec2{x=x, y=y}

var v = Vec2(1, 2)
```

### `$initRecord` method.
> _Planned Feature_

### `$initPair` method.
The `$initPair` method overrides the record initializer.
After an instance of the type is created from its default record initializer, this method is invoked for each key-value pair in the record literal:
```cy
type MyMap:
    func $initPair(self, key any, value any) void:
        print "$(key) = $(value)"

var m = MyMap{a=123, b=234}
--> a = 123
--> b = 234
```
`$initPair` is only allowed if the type has a default record initializer or `$initRecord` is declared.

### `$get` method.
The `$get` method allows overriding field accesses for **undeclared fields**:
```cy
type Foo:
    func $get(self, name String):
        return name.len()

var f = Foo{}
print f.abc      --> 3
print f.hello    --> 5
```

### `$set` method.
The `$set` method allows overriding field assignments for **undeclared fields**:
```cy
type Foo:
    func $set(self, name String, value any):
        print "setting $(name) $(value)"

var f = Foo{}
f.abc = 123      --> setting abc 123
```

### Missing method.
Declare a `$missing` method as a fallback when a method was not found in an instance.
> _Planned Feature_
```cy
type A:

    func $missing(self, args...):
        return args.len

var a = A{}
print a.foo()      -- Output: '0'
print a.bar(1, 2)  -- Output: '2'
```

## Reflection.
A [`type metatype`](#type-metatype) object references an internal type. Use the `typeof` builtin to get the `metatype` of a value.
```cy
var val = 123
print typeof(val)   -- 'type: float'

-- Referencing a type as a value also returns its `metatype`.
print bool          -- 'type: bool'
```

## Attributes.
Attributes start with `@`. They are used as declaration modifiers.
> `@host`
>
>Bind a function, variable, or type to the host. See [libcyber](#libcyber).

## Templates.
Templates enables parametric polymorphism for types and functions. Template arguments are passed to templates to generate specialized code. This facilitates developing container types and algorithms that operate on different types.

See [Custom Types / Type templates](#type-templates) and [Functions / Function templates](#function-templates).

### Template specialization.
> _Planned Feature_

## Macros.
> _Planned Feature_

## Compile-time execution.
> _Planned Feature_

### Builtin types.
Builtin types are used internally by the compiler to define it's own primitive types such as `bool`, `int`, and `float`.
> `type bool_t`
>

> `type int64_t`
>

> `type float64_t`
>

### Builtin functions.
> `func genLabel(name String)`
>
>Emits a label during codegen for debugging.

### Builtin constants.
> `var modUri String`
>
>Evaluates to the module's URI as a string. See [Module URI](#module-uri).

## Runtime execution.
`cy.eval` evaluates source code in an isolated VM.
If the last statement is an expression, a primitive or String can be returned to the caller:
```cy
use cy

var res = cy.eval('1 + 2')
print res        --> 3
```

# libcyber.

<table><tr>
<td valign="top">

* [Getting started.](#getting-started)
  * [Create VM.](#create-vm)
  * [Override `print`.](#override-print)
  * [Eval script.](#eval-script)
* [Module Loader.](#module-loader)
  * [Default module loader.](#default-module-loader)
  * [Bind functions.](#bind-functions)
  * [Variable loader.](#variable-loader)
  * [Bind types.](#bind-types)
</td><td valign="top">

* [Host functions.](#host-functions)
* [Host types.](#host-types)
  * [`getChildren`](#getchildren)
  * [`finalizer`](#finalizer-1)
</td>
</tr></table>

[^top](#table-of-contents)

`libcyber` allows embedding the Cyber compiler and VM into applications. Cyber's core types and the CLI app were built using the library.

The API is defined in the [C header file](https://github.com/fubark/cyber/blob/master/src/include/cyber.h).
The examples shown below can be found in the repository under [c-embedded](https://github.com/fubark/cyber/blob/master/examples/c-embedded). C is used as the host language, but it can be easily translated to C++ or any C-ABI compatible language.

Types and constants from the C-API begin with `CL` and functions begin with `cl`.

## Getting started.

### Create VM.
Most operations are tied to a VM handle. To create a new VM instance, call `clCreate`:
```c
#include "cyber.h"

int main() {
    CLVM* vm = clCreate();
    // ...
    clDestroy(vm);
    return 0;
}
```

### Override `print`.
The builtin `print` function does nothing by default, so it needs to be overrided to print to stdout for example:
```c
void printer(CLVM* vm, CLStr str) {
    printf("Invoked printer: %.*s\n", (int)str.len, str.buf);
}

int main() {
    // ...
    clSetPrinter(vm, printer);
    // ...
}
```
Note that `print` invokes the printer twice, once for the value's string and another for the new line character.

### Eval script.
`clEval` compiles and evaluates a script:
```c
CLStr src = STR(
    "var a = 1\n"
    "print(a + 2)\n"
);

CLValue val;
CLResultCode res = clEval(vm, src, &val);
if (res == CL_SUCCESS) {
    printf("Success!\n");
    clRelease(vm, val);
} else {
    CLStr report = clNewLastErrorReport(vm);
    printf("%s\n", report);
    clFree(vm, report);
}
```
If a value is returned from the main block of the script, it's saved to the result value argument.
Memory is managed by ARC so a value that points to a heap object requires a `clRelease` when it's no longer needed.

`clEval` returns a result code that indicates whether it was successful.

## Module Loader.
A module loader describes how a module is loaded when `use` import statement is encountered during script execution.
Only one module loader can be active and is set using `clSetModuleLoader`:
```c
bool modLoader(CLVM* vm, CLStr spec, CLModule* res) {
    if (strncmp("my_mod", spec.buf, spec.len) == 0) {
        CLStr src = STR(
            "@host func add(a float, b float) float\n"
            "@host var .MyConstant float\n"
            "@host var .MyList     List[dyn]\n"
            "\n"
            "@host\n"
            "type MyNode _:\n"
            "    @host func asList(self) any"
            "\n"
            "@host func MyNode.new(a any, b any) MyNode\n"
        );
        *res = clCreateModule(vm, spec, src);
        CLModuleConfig config = (CLModuleConfig){
            .funcs = (CLSlice){ .ptr = funcs, .len = 3 },
            .types = (CLSlice){ .ptr = types, .len = 1 },
            .varLoader = varLoader,
        };
        clSetModuleConfig(vm, *res, &config);
        return true;
    } else {
        // Fallback to the default module loader to load `core`.
        return clDefaultModuleLoader(vm, spec, out);
    }
}

int main() {
    //...
    clSetModuleLoader(vm, modLoader);
    //...
}
```
The above example checks whether "my_mod" was imported and returns it's source code. Additional loaders are returned to load the functions, variables, and types from the source code.

### Default module loader.
Since only one module loader can be set to the VM instance, a custom loader is required to handle the "core" import which contains all of the core types and functions in Cyber. This can simply be delegated to `clDefaultModuleLoader`.

### Bind functions.
An array of function definitions can be assigned to `CLModuleConfig.funcs`.
When a `@host` function is encountered by the compiler, it will use this mapping to find the correct function pointer:
```c
CLHostFuncEntry funcs[] = {
    CL_FUNC("add",           add),
    CL_FUNC("MyNode.asList", myNodeAsList),
    CL_FUNC("MyNode.new",    myNodeNew),
};
```

A fallback function loader can be assigned to `CLModuleConfig.func_loader`.
It's invoked if a function could not be found in `CLModuleConfig.funcs`.
```c
bool funcLoader(CLVM* vm, CLFuncInfo info, CLFuncResult* out) {
    // Check that the name matches before setting the function pointer.
    if (strncmp("missing_func", info.name.buf, info.name.len) == 0) {
        out->ptr = myMissingFunc;
        return true;
    } else {
        return false;
    }
}
```

### Variable loader.
A variable loader describes how to load a `@host` variable when it's encountered by the compiler:
```c
// C has limited static initializers (and objects require a vm instance) so initialize them in `main`.
typedef struct { char* n; CLValue v; } NameValue;
NameValue vars[2];

bool varLoader(CLVM* vm, CLVarInfo info, CLValue* out) {
    // Check that the name matches before setting the value.
    if (strncmp(vars[info.idx].n, info.name.buf, info.name.len) == 0) {
        // Objects are consumed by the module.
        *out = vars[info.idx].v;
        return true;
    } else {
        return false;
    }
}

int main() {
    // ...

    // Initialize var array for loader.
    vars[0] = (NameValue){".MyConstant", clFloat(1.23)};
    CLValue myInt = clInteger(123);
    vars[1] = (NameValue){".MyList", clNewList(vm, &myInt, 1)};

    // ...
}
```
This example uses the same technique as the function loader, but it can be much simpler. It doesn't matter how the mapping is done as long as the variable loader returns a `CLValue`.

### Bind types.
An array of type definitions can be assigned to `CLModuleConfig.types`.
When a `@host` type is encountered by the compiler, it will use this mapping to initialize the type:
```c
CLTypeId myNodeId;

CLHostTypeEntry types[] = {
    CL_CUSTOM_TYPE("MyNode", &myNodeId, myNodeGetChildren, myNodeFinalizer),
};
```
When binding to the "MyNode" type, it's type id is saved to `myNodeId`. This id is then used to create new instances of this type. See [Host types](#host-types).

A fallback type loader can be assigned to `CLModuleConfig.type_loader`.
It's invoked if a type could not be found in `CLModuleConfig.types`.
```c
bool typeLoader(CLVM* vm, CLTypeInfo info, CLTypeResult* out) {
    if (strncmp("MissingType", info.name.buf, info.name.len) == 0) {
        out->type = CS_TYPE_OBJECT;
        out->data.object.outTypeId = &myNodeId;
        out->data.object.getChildren = myNodeGetChildren;
        out->data.object.finalizer = myNodeFinalizer;
        return true;
    } else {
        return false;
    }
}
```

## Host functions.
A host function requires a specific function signature:
```c
CLValue add(CLVM* vm, const CLValue* args, uint8_t nargs) {
    double res = clAsFloat(args[0]) + clAsFloat(args[1]);
    return clFloat(res);
}
```
A host function should always return a `CLValue`. `clNone()` can be returned if the function does not intend to return any value.

## Host types.
A host type are types that are opaque to Cyber scripts but still behave like an object. They can have type functions and methods.

Only the host application can directly create new instances of them, so usually a function is binded to expose a constructor to the user script:
```c
// Binding a C struct with it's own children and finalizer.
// This struct retains 2 VM values and has 2 arbitrary data values unrelated to the VM.
typedef struct MyNode {
    CLValue val1;
    CLValue val2;
    int a;
    double b;
} MyNode;

// Implement the `new` function in MyNode.
CLValue myNodeNew(CLVM* vm, const CLValue* args, uint8_t nargs) {
    // Instantiate our object.
    CLValue new = clNewHostObject(vm, myNodeId, sizeof(MyNode));
    MyNode* my = (MyNode*)clAsHostObject(new);

    // Assign the constructor args passed in and retain them since the new object now references them.
    clRetain(vm, args[0]);
    my->val1 = args[0];
    clRetain(vm, args[1]);
    my->val2 = args[1];

    // Assign non VM values.
    my->a = 123;
    my->b = 9999.999;
    return new;
}
```
`clNewHostObject` takes the type id (returned from the [Type loader](#bind-types)) and size (in bytes) and returns a new heap object. Note that the size is allowed to vary. Different instances of the same type can occupy different amounts of memory.

### `getChildren`
Since `MyNode` contains `CLValue` children, the [Type loader](#bind-types) requires a `getChildren` callback so that memory management can reach them:
```c
CLValueSlice myNodeGetChildren(CLVM* vm, void* obj) {
    MyNode* my = (MyNode*)obj;
    return (CLValueSlice){ .ptr = &my->val1, .len = 2 };
}
```

### `finalizer`
A type finalizer is optional since the memory and children of an instance will be freed automatically by ARC.
However, it can be useful to perform additional cleanup tasks for instances that contain external resources.
```c
void myNodeFinalizer(CLVM* vm, void* obj) {
    printf("MyNode finalizer was called.\n");
}
```

# Memory.

<table><tr>
<td valign="top">

* [Structured memory.](#structured-memory)
  * [Value ownership.](#value-ownership)
  * [Copy semantics.](#copy-semantics)
  * [Cloning.](#cloning)
  * [Moving.](#moving)
  * [References.](#references)
  * [Exclusive reference.](#exclusive-reference)
  * [`self` reference.](#self-reference)
  * [Lifted values.](#lifted-values)
  * [Deferred references.](#deferred-references)
  * [Implicit lifetimes.](#implicit-lifetimes)
  * [Reference lifetimes.](#explicit-reference-lifetimes)
  * [Shared ownership.](#shared-ownership)
  * [Deinitializer.](#deinitializer)
  * [Pointer interop.](#pointer-interop)
</td><td valign="top">

* [Automatic memory.](#automatic-memory)
  * [ARC.](#arc)
  * [Object destructor.](#object-destructor)
  * [Retain optimizations.](#retain-optimizations)
  * [Closures.](#closures-1)
  * [Fibers.](#fibers-1)
  * [Heap objects.](#heap-objects)
  * [GC.](#gc)
* [Manual memory.](#manual-memory)
  * [Memory allocations.](#memory-allocations)
  * [Runtime memory checks.](#runtime-memory-checks)
</td>
</tr></table>

[^top](#table-of-contents)

Cyber provides memory safety by default with structured and automatic memory.
Manual memory is also supported but discouraged.

## Structured memory.
Cyber uses single value ownership and variable scopes to determine the lifetime of values.
When lifetimes are known at compile-time, the memory occupied by values do not need to be manually managed which prevents memory bugs such as:
* Use after free.
* Use after invalidation.
* Free with wrong allocator.
* Double free.
* Memory leaks.
* Null pointer dereferencing.

At the same time, structured memory allows performant code to be written since it provides safe semantics to directly reference values and child values.

### Value ownership.
Every value in safe memory has a single owner.
An owner can be a variable that binds to a value. Otherwise, the owner can be a parent value or the value itself.
The owner is responsible for deinitializing or dropping the value when it has gone out of scope (no longer reachable).
For example, at the end of the block, a variable can no longer be accessed so it drops the value that it owns:
```cy
var a = 123
print a    --> 123
-- Deinit `a`.
```
In this case, there is nothing to deinitialize since the value is an integer.

If the value was a `String`, the deinit logic would release (-1) on a reference counted byte buffer since strings are just immutable views over byte buffers:
```cy
var a = 'hello'
print a    --> hello
-- Deinit `a`.
-- `a.buf` is released.
-- `a.buf` is freed.
```
Since the string buffer's reference count reaches 0, it's freed as well.

Finally, let's take a look at `ListValue` which manages a dynamically sized array of elements:
```cy
var a = ListValue[int]{1, 2, 3}
print a    --> {1, 2, 3}
-- Deinit `a`.
-- `a.buf` is freed.
```
When `a` is deinitialized, the buffer that holds the 3 integer elements is freed.
You may have surmised that it's named `ListValue` because it's a value type (it can only be passed around by copying itself). The object type, [`List`](#lists), wraps `ListValue` and can be passed around by reference.

The concept of a value having a single owner is very simple yet powerful.
A value can represent any data structure from primitives to dynamically allocated buffers.
A value always knows **how** to deinitialize itself, and the owner knows **when** to deinitialize the value.
Later, we'll see that this same concept also applies to [shared ownership](#shared-ownership).

### Copy semantics.
By default, values are passed around by copying (shallow copying), but now all values can perform a copy.

A primitive, such as an integer, can always be copied:
```cy
var a = 123
var b = a
-- Deinit `b`.
-- Deinit `a`.
```
After a copy, a new value is created and given the owner of `b`. At the end of the block, both `a` and `b` are deinitialized (which does nothing since they are just primitives).

Strings are also copyable since they are immutable views over byte buffers:
```
var a = 'hello'
var b = a
-- Deinit `b`.
-- `b.buf` is released.
-- Deinit `a`.
-- `a.buf` is released.
-- `a.buf` is freed.
```
The copy `b` also reuses the byte buffer of `a` by retaining (+1) on the reference counted byte buffer. The byte buffer is finally freed once there are no references pointing to it.

Unlike the integer and string, a `ListValue` can not be copied since doing so requires duping a heap allocated buffer which is considered expensive:
```cy
var a = ListValue[int]{1, 2, 3}
var b = a      --> error: Can not copy `ListValue`. Can only be cloned or moved.
```
Instead `a` can only be [cloned](#cloned) or [moved](#moving).

By default, a declared value type is copyable if all of it's members are also copyable:
```cy
type Foo struct:
    a int
    b String

var a = Foo{a=123, b='hello'}
var b = a
```
Since integers and strings are both copyable, `Foo` is also copyable.

`Foo` is non-copyable if it contains at least one non-copyable member:
```cy
type Foo struct:
    a int
    b String
    c ListValue[int]

var a = Foo{a=123, b='hello'}
var b = a      --> error: Can not copy `Foo`. Can only be moved.
```

`Foo` is also non-copyable if it contains unsafe types such as pointers or pointer slices:
```cy
type Foo struct:
    a int
    b String
    c *Bar
    d [*]float
```

`Foo` can implement `Copyable` to override the default behavior and define it's own copy logic:
```cy
type Foo struct:
    with Copyable
    a int
    b String
    c *Bar
    d [*]float

    func copy(self) Foo:
        return .{
            a = self.a,
            b = self.b,
            c = self.c,
            d = self.d,
        }
```

Likewise, `Foo` can implement `NonCopyable` which indicates that it can never be copied:
```cy
type Foo struct:
    with NonCopyable
    a int
    b String
```

### Cloning.
Some value types are not allowed to be copied by default and must be cloned instead:
```cy
var a = ListValue[int]{1, 2, 3}
var b = a.clone()
```

Any `Copyable` type is also `Cloneable`. For example, performing a clone on an integer will simply perform a copy:
```cy
var a = 123
var b = a.clone()
```

A value type can implement `Cloneable` to override the default behavior and define it's own clone logic:
```cy
type Foo struct:
    with Cloneable
    a int
    b String

    func clone(self) Foo:
        return .{
            a = self.a + 1,
            b = self.b,
        }
```

Likewise, `Foo` can implement `NonCloneable` which indicates that it can never be cloned:
```cy
type Foo struct:
    with NonCloneable
    a int
    b String
```

### Moving.
Values can be moved, thereby transfering ownership from one variable to another:
```cy
var a = 123
var b = move a
print a     --> error: `a` does not own a value.
```

Some types such as `ListValue` can not be passed around by default without moving (or cloning) the value:
```cy
var a = ListValue[int]{1, 2, 3}
print computeSum(move a)    --> 6
```
In this case, the list value is moved into the `computeSum` function, so the list is deinitialized in the function before the function returns.

### References.
References are safe pointers to values.
Unlike unsafe pointers, a reference is never concerned with when to free or deinitialize a value since that responsibility always belongs to the value's owner.
They are considered safe pointers because they are guaranteed to point to their values and never outlive the lifetime of their values.

References grant **stable mutability** which allows a value to be modified as long as it does not invalidate other references.
**Multiple** references can be alive at once as long as an [exclusive reference](#exclusive-reference) is not also alive.

The `&` operator is used to obtain a reference to a value:
```cy
var a = 123
var ref = &a
ref.* = 234
print a        --> 234
```

A reference can not outlive the value it's referencing:
```cy
var a = 123
var ref = &a
if true:
    var b = 234
    ref = &b   --> error: `ref` can not oulive `b`.
```

A reference type is denoted as `&T` where `T` is the type that the reference points to:
```cy
var a = 123

func inc(a &int):
    a.* = a.* + 1
```

References allow stable mutation:
```cy
var a = ListValue[int]{1, 2, 3}
var third = &a[2]
third.* = 300
print a        --> {1, 2, 300}
```
The element that `third` points to can be mutated because it does not invalidate other references.

References however can not perform a unstable mutation.
An unstable mutation requires an exclusive reference:
```cy
var a = ListValue[int]{1, 2, 3}
var ref = &a
ref.append(4)  --> error: Expected exclusive reference.
```

### Exclusive reference.
An exclusive reference grants **full mutability** which allows a value to be modified even if could potentially invalidate unsafe pointers.

A **single** exclusive reference can be alive as long as no other references are also alive.
Since no other references (safe pointers) are allowed to be alive at the same time, no references can become invalidated.

The `&!` operator is used to obtain an exclusive reference to a value.
An exclusive reference type is denoted as `&!T` where `T` is the type that the reference points to.

`ListValue` is an example of a type that requires an exclusive reference for operations that can resize or reallocate its dynamic buffer:
```cy
var a = ListValue[int]{1, 2, 3}
a.append(4)
print a        --> {1, 2, 3, 4}
```
Note that invoking the method `append` here automatically obtains an exclusive reference for `self` without an explicit `&!` operator.

If another reference is alive before `append`, the compiler will not allow an exclusive reference to be obtained from `a`.
Doing so would allow `append` to potentially reallocate its dynamic buffer, thereby invalidating other references:
```cy
var a = ListValue[int]{1, 2, 3}
var third = &a[2]
a.append(4)    --> error: Can not obtain exclusive reference, `third` is still alive.
print third
```

### `self` reference.
By default `self` has a type of `&T` when declared in a value type's method:
```cy
type Pair struct:
    a int
    b int

    func sum(self) int:
        return self.a + self.b
```

If `self` requires an exclusive reference, then it must be prepended with `!`:
```cy
type Pair struct:
    a int
    b int

    func sum(!self) int:
        return self.a + self.b
```

Invoking methods automatically obtains the correct reference as specified by the method:
```cy
var p = Pair{a=1, b=2}
print p.sum()     --> 3
```

### Lifted values.
> _Planned Feature_

### Deferred references.
> _Planned Feature_

### Implicit lifetimes.
> _Planned Feature_

### Reference lifetimes.
> _Planned Feature_

### Shared ownership.
> _Planned Feature_

### Deinitializer.
> _Planned Feature_

### Pointer interop.
> _Planned Feature_

## Automatic memory.
Cyber uses an ARC/GC hybrid to automatically manage objects instantiated from object types. Value types typically do not need to be automatically managed unless they were lifted by a [closure](#closures-1) or a dynamic container.

### ARC.
ARC also known as automatic reference counting is deterministic and has less overhead compared to a tracing garbage collector. Reference counting distributes memory management, which reduces GC pauses and makes ARC suitable for realtime applications. One common issue in ARC implementations is reference cycles which Cyber addresses with a [GC](#gc) supplement when it is required.

Objects are managed by ARC. Each object has its own reference counter. Upon creating a new object, it receives a reference count of 1. When the object is copied, it's **retained** and the reference count increments by 1. When an object value is removed from it's parent or is no longer reachable in the current stack frame, it is **released** and the reference count decrements by 1.

Once the reference count reaches 0 the object begins its [destruction](#object-destructor) procedure. 

### Object destructor.
An object's destructor invoked from ARC performs the following in order:
1. Release child references thereby decrementing their reference counts by 1. If any child reference counts reach 0, their destructors are invoked.
2. If the object has a finalizer, it's invoked.
3. The object is freed from memory.

If the destructor is invoked by the GC instead of ARC, cyclable child references are not released in step 1.
Since objects freed by the GC either belongs to a reference cycle or branched from one, the GC will still end up invoking the destructor of all unreachable objects.
This implies that the destructor order is not reliable, but destructors are guaranteed to be invoked for all unreachable objects.

### Retain optimizations.
When the lifetime of an object's reference is known on the stack, a large amount of retain/release ops can be avoided.
For example, calling a function with an object doesn't need a retain since it is guaranteed to be alive when the function returns.
This leaves only cases where an object must retain to ensure correctness such as escaping the stack.

When using dynamic types, the compiler can omit retain/release ops when it can infer the actual type even though they are dynamically typed to the user.

### Closures.
When primitive variables are captured by a [closure](#closures), they are boxed and allocated on the heap. This means they are managed by ARC and cleaned up when there are no more references to them.

### Fibers.
[Fibers](#fibers) are freed by ARC just like any other object. Once there are no references to the fiber, it begins to release it's child references by unwinding it's call stack.

### Heap objects.
Many object types are small enough to be at or under 40 bytes. To take advantage of this, object pools are reserved to quickly allocate and free these small objects with very little bookkeeping. Bigger objects are allocated and managed by `mimalloc` which has proven to be a fast and reliable general-purpose heap allocator.

### GC.
The garbage collector is only used if the program may contain objects that form reference cycles. This property is statically determined by the compiler. Since ARC frees most objects, the GC's only responsibility is to free abandoned objects that form reference cycles.
This reduces the amount of work for GC marking since only cyclable objects (objects that may contain a reference cycle) are considered.

Weak references are not supported for object types because objects are intended to behave like GC objects (the user should not be concerned with reference cycles). If weak references do get supported in the future, they will be introduced as a `Weak[T]` type that is used with an explicit reference counted `Rc[T]` type.

Currently, the GC can be manually invoked. However, the plan is for this to be automatic by either running in a separate thread or per virtual thread by running the GC incrementally.

To invoke the GC, call the builtin function: `performGC`. *Incomplete Feature: Only the main fiber stack is cleaned up at the moment.*
```cy
func foo():
    -- Create a reference cycle.
    var a = {_}
    var b = {_}
    a.append(b)
    b.append(a)

    -- Cycle still alive in the current stack so no cleanup is done.
    var res = performGC()
    print res['numCycFreed']    -- Output: 0
    print res['numObjFreed']    -- Output: 0

foo()
-- `a` and `b` are no longer reachable, so the GC does work.
var res = performGC()
print res['numCycFreed']      -- Output: 2
print res['numObjFreed']      -- Output: 2
```

## Manual memory.
> _Planned Feature_

### Memory allocations.
> _Planned Feature_

### Runtime memory checks.
*Planned Feature*
When runtime memory checks are enabled, the compiler will insert traps and runtime logic to prevent the following unsafe uses of pointers and memory allocations:
* Use after free.
* Double free.
* Out of bounds memory access.
* Null pointer access.
* Misaligned pointer access.
* Unfreed memory.

With these checks in place, using manual memory will be much less error prone at the cost of some runtime performance and memory cost. Pointers will occupy an extra word size and the runtime will maintain an allocation table that contains the raw pointer and metadata.

# CLI.

* [Basic commands.](#basic-commands)
* [REPL.](#repl)
* [JIT compiler.](#jit-compiler)
* [C backend.](#c-backend)

[^top](#table-of-contents)

## Basic commands.
To compile and run a program with the VM, provide the path to the main Cyber source file:
```bash
cyber foo.cy
cyber path/to/main.cy
```

To see more options and commands, print the help screen:
```bash
cyber help

# These are aliases to the help command.
cyber -h
cyber --help
```

## REPL.
The REPL is started by running the CLI without any arguments:
```bash
cyber
```

The REPL starts new sessions with [`use $global`](#use-global). This allows undeclared variables to be used similar to other dynamic languages:
```bash
> a = 123
> a * 2
`int` 246
```

When the first input ends with `:`, the REPL will automatically indent the next line. To recede the indentation, provide an empty input. Once the indent returns to the beginning, the entire code block is submitted for evaluation:
```bash
> if true:
    | print 'hello!'
    | 
hello!
```

Top level declarations such as imports, types, and functions can be referenced in subsequent evals:
```bash
> use math
> math.random()
`float` 0.3650744641604983
```

```bash
> type Foo:
    | a int
    |
> f = Foo{a=123}
> f.a
`int` 123
```

Local variables **can not** be referenced in subsequent evals, since their scope ends with each eval input:
```bash
> var a = 123
> a
panic: Variable is not defined in `$global`.

input:1:1 main:
a
^
```

## JIT compiler.
Cyber's just-in-time compiler is incomplete and unstable. To run your script with JIT enabled:
```bash
cyber -jit &lt;script&gt;
```

The goal of the JIT compiler is to be fast at compilation while still being significantly faster than the interpreter. The codegen involves stitching together pregenerated machine code that targets the same runtime stack slots used by the VM. This technique is also known as `copy-and-patch`. As the VM transitions to unboxed data types, the generated code will see more performance gains.

## C backend.
The C backend generates a static binary from Cyber source code by first transpiling to C code and relying on a C compiler to produce the final executable.
The user can specify the system's `cc` compiler or the builtin `tinyc` compiler that is bundled with the CLI.
*This is currently in progress.*

&nbsp;

&nbsp;