# Table of Contents.
- [Introduction.](#introduction)
- [Syntax.](#syntax)
- [Basic types.](#basic-types)
- [Custom types.](#custom-types)
- [Control flow.](#control-flow)
- [Functions.](#functions)
- [Memory.](#memory)
- [Error handling.](#error-handling)
- [Concurrency.](#concurrency)
- [Metaprogramming.](#metaprogramming)
- [Modules.](#modules)
- [FFI.](#ffi)
- [libcyber.](#libcyber)
- [CLI.](#cli)

<!--TOC-END-->

<!--This does not contain module docs. Use docs.md from releases instead or generate it by using docs/gen-docs.cy -->

# Introduction.

Cyber is a safe, fast, and concurrent programming language.

The areas of focus for the language include:
* Static typing with primitives, references, structs, options, enums, choices, traits, and generic types.
* Memory safety. Favors structured and flat memory use through borrowing and reference counting. (No garbage collector.)
* Value ownership and deinitializers.
* Metaprogramming with templates, inline evaluation, and compiler hooks.
* Concurrency. Channels, futures, and `await` to synchronize green threads. Fibers and generators for cooperative coroutines.
* Compilation ahead-of-time to C or interpeted from its virtual machine.
* Sandbox mode that restricts the language to only safe constructs.
* Introspection. Much of the internals such as the builtin types and runtime is written in Cyber and can be inspected from the source code or the [API docs](api.html).

These docs provide a reference manual for the language. It can be read in order or browsed from the navigation. Features that are marked `Incomplete`, `Planned`, or `TBD` have not been completed at the time of writing.

## Hello World.
Here is a simple example that offers a sneak peek into the language:
```cy
use math

nouns := []str{'World', '世界', 'दुनिया', 'mundo'}
nouns += math.random().fmt()
for nouns |n|:
    print('Hello, %{n}!')
```

# Syntax.

<table><tr>
<td valign="top">

* [Statements.](#statements)
* [Comments.](#comments)
* [Blocks.](#blocks)
* [Variables.](#variables)
  * [Local variables.](#local-variables)
  * [`var` declaration.](#var-declaration)
  * [Variable scopes.](#variable-scopes)
  * [`global` variables.](#global-variables)
* [Constants.](#constants)
</td><td valign="top">

* [Reserved identifiers.](#reserved-identifiers)
  * [Keywords.](#keywords)
  * [Contextual keywords.](#contextual-keywords)
  * [Literals.](#literals)
* [Operators.](#operators)
  * [Arithmetic operators.](#arithmetic-operators)
  * [Comparison operators.](#comparison-operators)
  * [Logic operators.](#logic-operators)
  * [Bitwise operators.](#bitwise-operators)
  * [Operator overloading.](#operator-overloading)
</td>
</tr></table>

[^top](#table-of-contents)

## Statements.
A statement ends with the new line character:
```cy
a := 123
```

Any token inside delimited parentheses, brackets, or braces, can be wrapped to the next line:
```cy
sum := add(1, 2, 3, 4,
    100, 200, 300, 400)

colors := {'red', 'blue', 'green',
    'purple', 'orange', 'yellow'}
```

A statement can wrap to the next line if the last token before the new line is an operator or keyword.
```cy
gameover := health <= 0 or
    player.collides_with(spikes)

if year > 2020 and year <= 2030 and
    month > 0 and month <= 11:
    print('Valid')
```

## Comments.
A single line comment starts with two hyphens and ends at the end of the line.
```cy
-- This is a comment.

a := 123   -- This is a comment on the same line as a statement.
```

## Blocks.
Some statements can start a new block with a colon.
The first statement in a new block must be indented further.
Indentation can be spaces or tabs but not both.
```cy
-- This `if` statement begins a new block.
if true:
    a := 234
```

Subsequent statements in the block must follow the same indentation.
The block ends when a statement recedes from this indentation:
```cy
items := {10, 20, 30}
for items |it|:
    if it == 20:
        print(it)

    -- This is the first statement outside of the `if` block.
    print(it)
```

A block with a single statement can be written in a single line:
```cy
-- A single line block.
if true: print(123)

if true: print(123)
    -- Indentation error. The `if` block already ended.
    print(234)
```

Since blocks require at least one statement, `pass` can be used as a placeholder statement:
```cy
fn foo():
    pass
```

## Variables.
Variables allow values to be stored as named locations in memory.

### Local variables.
`:=` declares a variable with the type inferred from the initializer.
```cy
a := 123
```

Variables can be assigned afterwards using the `=` operator:
```cy
a = 234
```

### `var` declaration.
When a local variable is declared with `var`, a type specifier is required.
The initializer must satisfy the type constraint:
```cy
-- Correct.
var a float = 123.0

-- CompileError. Expected `float`, got `str`.
var b float = 'hello'
```

Sometimes, the `var` declaration can be simpler and more readable than an equivalent `:=` declaration:
```cy
var action ?str = switch color:
    case .green => 'go'
    case .red => 'stop'
    else => none
```

### Variable scopes.
Local variables exist until the end of their scope. Each block has their own variable scope.

Variables declared in the current scope will take precedence over any parent variables with the same name. This is also known as variable shadowing:
```cy
fn foo():
    a := 234

    if true:
        -- New `a` declared.
        a := 345

        print(a) 
        --> 345

    print(a)
    --> 234
```

### `global` variables.
Global variables live until the end of the program and can be accessed from any context (unless they have a private modifier). Globals are considered *unsafe* and are forbidden in sandbox mode.

Global variables are declared with `global` and require a type specifier:
```cy
global a int = 123

fn foo():
    print(a)    --> 123
```

The initializer of a global variable cannot reference other global variables:
```cy
global a int = 123

global b int = a
--> error: Initializer can not reference a global variable.
```

However, they can be reassigned afterwards to any runtime expression:
```cy
global b int = 0

b = a
```

Global variable initializers have a natural order based on when it was encountered by the compiler.
The following would invoke `load_a` before `load_b`:
```cy
global a int = load_a()
global b int = load_b()
```

Circular references are not possible because a global initializer cannot reference another global variable:
```cy
global a = b
--> error: Initializer can not reference a global variable.

global b = a     
```

## Constants.
Constants are declared with a const evaluated expression:
```cy
const pi float = 3.14159265358979323846264338327950288419716939937510
```

The type specifier is optional and can be inferred from the expression:
```cy
const empty = ''

const lib = switch meta.system():
    case .linux => 'mylib.so'
    case .windows => 'mylib.dll'
    case .macos => 'mylib.dylib'
    else => meta.unsupported()
```

## Reserved identifiers.

### Keywords.
There are `23` general keywords. This list categorizes them:

- [Control flow](#control-flow): [`if`](#if-statement) [`else`](#if-statement) [`switch`](#switch-matching) [`case`](#switch-matching) [`while`](#conditional-while) [`for`](#for-range) [`break`](#break-statement) [`continue`](#continue-statement) `pass` [`or`](#or-expression) [`and`](#and-expression)
- [Operators](#operators): `not`
- [Variables](#variables): [`var`](#local-variables) [`global`](#global-variables)
- [Functions](#functions): `fn` `return`
- [Ownership](#value-ownership): [`move`](#moving)
- [Concurrency](#concurrency): [`yield`](#generators)
- [Types](#custom-types): `type` `with`
- [Error handling](#error-handling): [`try`](#unwrap-block)
- [Modules](#modules): `use`

### Contextual keywords.
These keywords only have meaning in a certain context.
- [Methods](#methods): `self` `Self`
- [Types](#custom-types): [`struct`](#structs) [`cstruct`](#c-structs) [`enum`](#enums) [`cunion`](#c-unions) [`trait`](#traits)
- [Ownership](#value-ownership): [`scope`](#scope-parameter) [`sink`](#sink-parameter)
- [Functions](#functions): `void`

### Literals.
- [Boolean literal](#booleans): `true` `false`
- [Error literal](#error-value): `error`
- None: `none`
- Undefined: `undef`

## Operators.
The following operators are supported.
They are ordered from highest to lowest precedence.
All infix operators have left-to-right associativity and all prefix operators have the same precedence and right-to-left associativity.

|Operator|Kind|Description|
|---|---|---|
| `()` | special | Grouping. |
| `.` | infix | Accessor. |
| `[]` | special | Indexing. |
| `as` | prefix | Type casting. |
| `!` | prefix | Logic not. |
| `~` | prefix | Bitwise not. |
| `^` | prefix | Lift. |
| `&` | prefix | Borrow. |
| `*` | prefix | Address of. |
| `<<` `>>` | infix | Bitwise left shift, right shift. |
| `&&` | infix | Bitwise and. |
| `||` `~` | infix | Bitwise or, exclusive or. |
| `**` | infix | Power or repeat. |
| `/` `%` `*` | infix | Division, modulus, multiplication. |
| `+` `-` | infix | Addition, subtraction. |
| `>` `>=`<br/>`<` `<=`</br>`!=` `==` | infix | Greater, greater or equal, less, less or equal, not equals, equals. |
| `and` | infix | Logical and. |
| `or` | infix | Logical or. |
| `..` | infix | Range. |

### Arithmetic operators.
The following arithmetic operators are supported for [numeric data types](#numbers).
Some types such as `math.Vec` and `math.Mat` also overload these operators.
```cy
1 + 2     --> 3   (Addition)
100 - 10  --> 90  (Subtraction)
3 * 4     --> 12  (Multiplication)
20 / 5    --> 4   (Division)
2 ** 4    --> 16  (Power)
12 % 5    --> 2   (Modulus remainder)
-(10)     --> -10 (Negative)
```

### Comparison operators.
The following comparison operators are supported and evaluate to a [Boolean](#booleans) value.

The `==` equals operator returns true if the two values are equal.
Primitive values compare with their underlying bytes.
The comparison recurses for composite types.
For the `str` type, the underlying bytes are compared for equality.
For references types, the comparison checks that the two references point to the same value.
```cy
1 == 1          --> true
1 == 2          --> false
1 == true       --> false

a := 'abc'
a == 'abc'      --> true

la := {1, 2, 3}
lb := la
la == lb        --> true
la == {1, 2, 3} --> false
```

The not equals operator returns true if the two values are not equal.
```cy
1 != 1          --> false
1 != 2          --> true
```

Number types have additional comparison operators.
```cy
a > b           --> `true` if a is greater than b
a >= b          --> `true` if a is greater than or equal to b
a < b           --> `true` if a is less than b
a <= b          --> `true` if a is less than or equal to b
```

### Logic operators.

The logical operators [`and`](#and-expression), [`or`](#or-expression), and `not` are supported.

The unary operators `not` and `!` perform negation on the boolean value:
```cy
not false     --> true
not true      --> false
!false        --> true
!true         --> false
```

### Bitwise operators.

The following bitwise operators are supported for `Int` and `Raw` number values.
```cy
-- and: any underlying bits that are set in both integers are set in the new integer.
a && b

-- or: any underlying bits that are set in either integer a or integer b are set in the new integer.
a || b

-- exclusive or: any underlying bits that are set in either integer a or integer b but not both are set in the new integer.
a ~ b

-- logical right shift: a's bits are shifted b bits to the least significant end. 
a >> b

-- logical left shift: a's bits are shifted b bits to the most significant end. This does not perform sign-extension on the 32-bit integer.
a << b

-- not: a's integer bits are flipped.
~a
```

### Operator overloading.
See [Custom types -> Operator methods](#operator-methods).

# Basic types.

<table><tr>
<td valign="top">

* [Booleans.](#booleans)
* [Numbers.](#numbers)
  * [Integers.](#integers)
  * [Floats.](#floats)
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
* [References.](#references)

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
* [Vectors.](#vectors)
  * [Partial vectors.](#partial-vectors)
* [Slices.](#slices)
  * [Sub-slices.](#sub-slices)
  * [Slice operations.](#slice-operations)
* [Maps.](#maps)
  * [Map operations.](#map-operations)
* [Implicit casts.](#implicit-casts)
* [Type casts.](#type-casts)
</td>
</tr></table>

[^top](#table-of-contents)

This chapter is an overview of commonly used builtin types.

## Booleans.
A `bool` type can be `true` or `false`.
```cy
a := true
if true:
    print('a is true')
```

## Numbers.

### Integers.
`int` (an alias for `i64`) is the default integer type.
It's encoded as 64-bit two's complement and represents integers in the range -(2<sup>63</sup>) to 2<sup>63</sup>-1.

Integer types are named according to how many bits they occupy: `i8`, `i16`, `i32`, `i64`

Raw integer types do not encode a sign bit. They include: `r8` (`byte`, `r16`, `r32`, `r64`. `byte` is an alias for `r8`.

While integer types are typically used for counting, raw integer types are intended to represent masks and raw memory.

Without a target type, a numeric literal will default to the `int` type:
```cy
a := 123
```

Other integer notations include:
```cy
a := 0xFF     -- Hexidecimal.
a = 0o17      -- Octal.
a = 0b1010    -- Binary.
```

String literals evaluate as their UTF-8 codepoint if the target integer type is big enough:
```cy
-- Success.
var a int = '🐶'

-- error: Expected `byte`, found `str`.
var b byte = '🐶'

-- Success.
var b byte = 'a'
```

Strings and other values can be converted to a `int` using the type as a function:
```cy
a := '123'
b := int(a) 
```

Integer types can perform [arithmetic](#arithmetic-operators), [bitwise](#bitwise-operators), and [comparison](#comparison-operators) operations.

### Floats.
`float` (an alias for `f64`) is the default floating point type. It's encoded as 64-bit IEEE 754.

Floating point types include: `f32`, `f64`

A `float` can represent integers between -(2<sup>53</sup>-1) and (2<sup>53</sup>-1). However, integers outside this range are not guaranteed to have a unique representation.

Decimal and scientific notations always produce a `float` value:
```cy
a := 2.34567
b := 123.0e4
```

An integer literal can evaluate to a target float type:
```cy
var a float = 123
```

Strings and other values can be converted to a `float` using the type as a function:
```cy
a := '12.3'
b := float(a) 
```

Float types can perform [arithmetic](#arithmetic-operators) and [comparison](#comparison-operators) operations.

## Strings.
The `str` type represents a sequence of UTF-8 code points. Each code point is stored internally as 1-4 bytes.

Strings are not validated by default. When indexing for code points, an invalid codepoint will be returned as the replacement character (0xFFFD).

Strings are **immutable**, so an operation on a string returns a new string.

Some string operations are SIMD accelerated.

### Raw string literal.
A raw string doesn't allow any escape sequences or string interpolation.

Backticks are used to delimit a single line literal:
```cy
fruit := `apple`
s := `abc🦊xyz🐶`
```

Since raw strings interprets the sequence of characters as is, the backtick character can not be escaped:
```cy
-- ParseError.
s := `abc`xyz`
```

Triple backticks are used to delimit a multi-line literal. It also allows single backticks:
```cy
s := ```abc`xyz```
greet := ```Hello
World```
```

### String literal.
A string literal allows escape sequences and string interpolation.

Single or double quotes are used to delimit a single line literal:
```cy
fruit := 'apple'
fruit = 'Miso\'s apple'
fruit = "Miso's apple"
sentence := '%{fruit} is tasty.'
```

Triple single or double quotes are used to delimit a multi-line literal:
```cy
title := 'last'
doc := '''A single quote ' doesn't need to be escaped.'''
s := """line a
line "b"
line %{title}
"""
```

### Escape sequences.
The following escape sequences are supported in string literals:

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
| `\x??` | -- | Hexidecimal number |

Example:
```cy
print('\xF0\x9F\x90\xB6')    --> 🐶
```

### String indexing.
The index operator returns the `byte` from a given index:
```cy
a := 'abcxyz'
print(a[1])         --> 0x62
print(a[1] == 'b')  --> true
```

Since indexing operates at the byte level, it should not be relied upon for iterating runes or rune indexing.
However, if the string is known to only contain ASCII runes (each rune occupies one byte), indexing will return the expected rune.

`str.rune_at` returns the rune from a given byte index:
```cy
a := '🐶abcxyz'
print(a.rune_at(0)) --> 0x1f436
```

If the index does not begin a sequence of valid UTF-8 bytes, the replacement character (0xFFFD, 65533) is returned:
```cy
a := '🐶abcxyz'
print(a.rune_at(1)) --> 0xfffd
```

`str.seek` will return the n'th rune:
```cy
a := '🐶abcxyz'
print(a.seek(2))    --> 0x62 ('b')
```

Slicing also operates on byte indexes and returns a view of the string at the given start and end (exclusive) indexes:
```cy
a := 'abcxyz'
b := a[0..3]
print(a[0..3])  --> abc
print(a[1..])   --> bcxyz
```

### String concatenation.
Concatenate two strings together with the `+` operator or `str.concat`. 
```cy
res := 'abc' + 'xyz'
res = res.concat('end')
```

### String interpolation.
String templates wrap expressions in `%{}` which converts them to strings using the builtin `to_print_string`:
```cy
name := 'Rex'
points := 123
title := 'Scoreboard: %{name} %{points}'
```
String templates can not contain nested string templates.

### String formatting.
Formatting replaces `{}` placeholders with values converted to strings:
```cy
print('First: {}, Last: {}'.fmt({'John', 'Doe'}))
```

Alternatively, a custom placeholder can be specified:
```cy
print('if (%PH) {\n\t%PH}'.fmt('%PH', {cond, body}))
```

*Named placeholders will be supported.*

Values that can be formatted into a string typically have a `fmt` method:
```cy
x := 123
print(x.fmt(.hex))  --> 7b
```

### Line-join literal.
The line-join literal joins string literals with the new line character `\n`. *Planned Feature*

This has several properties:
* Ensures the use of a consistent line separator: `\n`
* Allows lines to have a mix of raw string or string literals.
* Single quotes and double quotes do not need to be escaped.
* Allows each line to be indented along with the surrounding syntax.
* Allows comments in between adjacent lines.
* The starting whitespace for each line is made explicit.

```cy
paragraph := {
    \`raw string literal
    \hello\nworld
    \hello %{name}
    -- This is a comment.
    \last line
    \
}
```

### Mutable strings.
`Str` is a mutable string type. *Planned Feature*

## Symbols.
Symbol literals begin with `@`, followed by an identifier.
Each symbol has a global unique ID.
```cy
currency := @usd
print(currency == @usd)   --> true
print(int(currency))      --> <unique ID>
```

## References.
A reference type is denoted as `^T` where `T` is the type of the value that the reference points to. 
References are safe to use because the memory that they point to are [automatically managed](#automatic-memory).

Separating values and references provides more control over how data is laid out in memory. For example, some types may benefit from compacted members to leverage cache locality and reduce object indirection.

The lift operator `^` lifts a value onto the heap and returns a reference to the new value:
```cy
i := 123
ref := ^i
```

The `.*` operator dereferences a `^T` and returns the value that it points to:
```cy
print(ref.*)      --> 123
```

If a value was intended to be initialized on the heap, it's usually constructed with the lift operator `^`. This avoids a copy of the underlying value:
```cy
pos := ^Vec2{x=4, y=5}
```

## Optionals.
An `Option` is a value type that provides **null safety** by forcing the inner value to be unwrapped before it can be used.

Option types either hold a `none` value or wraps `some` value.

A type prefixed with `?` is the idiomatic way to declare an option type. The following `str` optional types are equivalent:
```cy
Option[str]
?str
```

### Wrap value.
A value is automatically wrapped into the inferred optional's `some` case:
```cy
var a ?str = 'abc'
print(a)    --> abc
```

The option type's constructor can also wrap a value:
```cy
a := ?str('abc')
print(a)    --> abc
```

### Wrap `none`.
`none` is automatically initialized to the inferred optional's `none` case:
```cy
var a ?str = none
print(a)    --> none
```

The option type's constructor can also wrap `none`:
```cy
a := ?str(none)
print(a)    --> none
```

### Unwrap or panic.
The `.?` operator unwraps an optional. The current thread panics if the expression evaluates to the `none` case at runtime:
```cy
var opt ?int = 123

-- Success.
v := opt.?

opt = none

-- panic: Option is empty.
v = opt.?
```

### Unwrap or default.
The `?else` operator either returns the unwrapped value or a default value when the optional is `none`:
```cy
var opt ?int = none
var v = opt ?else 123
print(v)     --> 123
```

An `?else` block executes a block of statements for the `none` case:
```cy
value := opt ?else:
    return error.Missing
```

A value can be returned with `break`: *Planned Feature*
```cy
value := opt ?else:
    break 'empty'
```

### Optional chaining.
Given the last member's type `T` in a chain of `?.` operators, the expression will evaluate to `?T(none)` upon the first encounter of `none` or the value of the last member as `?T`: *Planned Feature*
```cy
last := root?.a?.b?.c?.last
```

### `if` unwrap.
The `if` statement can be amended to unwrap an optional value with the capture `|_|` clause:
```cy
var opt ?str = 'abc'
if opt |value|:
    print(value)    --> abc
```

### `while` unwrap. 
The `while` statement can be amended to unwrap an optional value using the capture `|_|` clause.
The loop exits when `none` is encountered:
```cy
iter := dir.walk()
while iter.next() |entry|:
    print(entry.name)
```

## Vectors.
A vector type is a static data structure that holds contiguous elements of the same type.
It's denoted as `[N]T` where `N` is the size of the vector and `T` is the element type.

Vectors are constructed with the initializer expression:
```cy
a := [3]int{1, 2, 3}
```

The number of elements can be inferred with the generic type `[_]T`:
```cy
a := [_]int{1, 2, 3}
```

An initializer literal can infer the target vector type:
```cy
var a [3]int = {1, 2, 3}
```

Vectors can be indexed:
```cy
a[2] = 300
print(a[2])    --> 300
```

### Partial vectors.
Sometimes a vector prefers to be incrementally initialized where the uninitialized elements are unowned. The partial vector type denoted as `[..N]T` keeps track of how many elements in the vector are initialized at runtime so that it can be properly deinitialized:
```cy
elems := [..4]Stateful{}
print(elems.len())   --> 0

-- No `Stateful` element is deinitialized.
```

Elements must be incrementally appended to the `PartialVector`. Indexing an element that hasn't been initialized results in a thread panic:
```cy
elems << Stateful()
elems << Stateful()

elems[3] = Stateful()
--> panic: Out of bounds.
```

## Slices.
A `Slice` type is a dynamic data structure that holds contiguous elements of the same type.
It's denoted as `[]T` where `T` is the element type.
Slices grow or shrink when inserting or removing elements.

Slices are constructed with the initializer expression:
```cy
arr := []int{1, 2, 3}
```

The intializer literal can infer the target slice type:
```cy
var a []int = {1, 2, 3}
```

The first element of the slice starts at index 0.
```cy
print(arr[0])    --> 1
```

### Sub-slices.
Slices can be sliced into smaller sub-slices. Sub-slices share the same underlying element buffer as the original slice which enables read/write access to the same elements:
```cy
arr := []int{1, 2, 3, 4, 5}
print(arr[0..0])    --> {}
print(arr[0..3])    --> {1, 2, 3}
print(arr[3..])     --> {4, 5}
```

A sub-slice will clone the underlying buffer upon any resize operation such as `append`, `insert`, `remove`, etc.
Once cloned, the slice will no longer point to the same elements as the original slice:
```cy
arr := []int{1, 2, 3}
slice := arr[0..1]
slice[0] = 100
print('%{arr[0]} %{slice[0]')   --> 100 100

slice += 4
slice[0] = 101
print('%{arr[0]} %{slice[0]')   --> 100 101
```

The `+..` invokes the slice operator with an end position that is an increment from the start: *Planned Feature*

```cy
arr := []int{1, 2, 3, 4, 5}
print(arr[2+..2])   --> {3, 4}
```

### Slice operations.
Here are some common slice operations:
```cy
arr := []int{234}

-- Append a value.
arr += 123

-- Alternative way to append.
arr = arr.append(123)

-- Inserting a value at an index.
arr = arr.insert(1, 345)

-- Get the length.
print(arr.len())  --> 2

-- Sort the slice in place.
arr.sort(|a, b| a < b)

-- Iterating a slice.
for arr |it|:
    print(it)

-- Remove an element at a specific index.
arr = arr.remove(1)
```

## Maps.
A `Map` type is a dynamic data structure that stores key value pairs in a lookup table governed by hashing functions (key hash and equality functions).
`Map` provides default hashing functions while `HashMap` requires custom hash functions.

Maps are constructed with the initializer expression:
```cy
map := Map[str, int]{a=123, b=234}
```

The initializer literal can infer the target map type:
```cy
var map Map[str, int] = {a=123, b=234}
```

### Map operations.
Here are some common map operations:
```cy
map := Map[int, int]{}

-- Set a key value pair.
map[123] = 234

-- Lookup value by key.
print(map[123])     --> 234

-- Get the size of the map.
print(map.size())   --> 1

-- Remove an entry by key.
map.remove(123)

-- Iterating a map.
for map |entry|:
    print('%{entry.key} -> %{entry.value}')
```

## Implicit casts.
Closely related types are implicitly casted to fit the target type:
```cy
var f float = 1.23
var i int = 123

-- Implicit cast from `int` to `float`.
f = i
```

Implicit casts avoid lossy conversions (with the exception of int to float)
and they never produce runtime errors.
The following implicit casts are supported:

| Source | Target | Behavior |
| --- | --- | --- |
| `Int[W]` | `Raw[W]` | reinterpret |
| `Int[X]` | `Raw[W] where X < W` | zero extension |
| `Raw[X]` | `Raw[W] where X < W` | zero extension |
| `Raw[W]` | `Int[W]` | reinterpret |
| `Raw[X]` | `Int[W] where X < W` | zero extension |
| `Int[X]` | `Int[W] where X < W` | sign extension |
| `Int[X]` | `Float[W]` | conversion |
| `Raw[X]` | `Float[W]` | conversion |
| `f32` | `f64` | conversion |
| `T` | `?T` | wrap |
| `T` | `!T` | wrap |
| `error` | `!T` | wrap |
| `T` | `!?T` | wrap |
| `^T` | `&T` | reinterpret |
| `^T` | `Object` | reinterpret |
| `FuncPtr[Sig]` | `Func[Sig]` | wrap |
| `&S` | `&Dyn[T] where S implements T` | wrap |
| `^S` | `^Dyn[T] where S implements T` | wrap |
| `Ptr[T]` | `Ptr[void]` | reinterpret |
| `&T` | `Ptr[void]` | reinterpret |
| `Ptr[void]` | `Ptr[T]` | reinterpret |
| `&[N]T` | `Ptr[T]` | reinterpret |
| `&[..N]T` | `Ptr[T]` | reinterpret |
| `Ptr[Int[W]]` | `Ptr[Raw[W]]` | reinterpret |
| `Ptr[Raw[W]]` | `Ptr[Int[W]]` | reinterpret |
| `&T` | `Ptr[T]` | reinterpret |
| `&Int[W]` | `Ptr[Raw[W]]` | reinterpret |
| `&Raw[W]` | `Ptr[Int[W]]` | reinterpret |

## Type casts.
The `as` operator casts a value to a supported target type:
```cy
var i int = 127
var small i8 = 0

-- Success.
small = as[i8] i
```

Some casts can fail at runtime. For example, a bigger integer can only be casted to a smaller integer if its value fits the bounds of the smaller integer:
```cy
var i int = 10000
var small i8 = 0

-- panic: Lossy conversion.
small = as[i8] i
```

When the target type can be inferred, it can be omitted from the `as` operator:
```cy
small = as i
```

Type casting supports all [implicit casts](#implicit-casts) in addition to the following:

| Source | Target | Behavior |
| --- | --- | --- |
| `Ptr[S]` | `Ptr[T]` | reinterpret |
| `i64`, `r64` | `Ptr[T]` | reinterpret |
| `Int[W]`, `Raw[W]` | `Ptr[T] where W < 64` | zero extension |
| `Ptr[T]` | `&T` | reinterpret |
| `Ptr[T]` | `^T` | reinterpret |
| `FuncPtr[Sig2]` | `FuncPtr[Sig] where #[extern] Sig, Sig2` | reinterpret |
| `Ptr[T]` | `i64` | reinterpret |
| `^T` | `i64` | reinterpret |
| `T` | `i64 where T is enum` | reinterpret |
| `Int[X]` | `Int[W] where X > W` | runtime check, convert |
| `Float[X]` | `Int[W]` | runtime check, convert |
| `Ptr[T]` | `r64` | reinterpret |
| `^T` | `r64` | reinterpret |
| `T` | `r64 where T is enum` | reinterpret |
| `Int[X]` | `Raw[W] where X > W` | runtime check, convert |
| `f64` | `f32` | convert |
| `^Dyn[S]` | `^T where T implements S` | runtime check, unwrap |

# Custom types.
<table><tr>
<td valign="top">

* [Structs.](#structs)
  * [Initialize struct.](#initialize-struct)
  * [Default field values.](#default-field-values)
  * [Field visibility.](#field-visibility)
  * [Circular references.](#circular-references)
  * [Type embedding.](#type-embedding)
* [`@init`.](#@init)
* [`@init_sequence`.](#@init_sequence)
* [`@init_record`.](#@init_record)
* [Methods.](#methods)
  * [Operator methods.](#operator-methods)
  * [Special methods.](#special-methods)
* [Tuples.](#tuples)
* [Type namespace.](#type-namespace)
* [Type aliases.](#type-aliases)
</td><td valign="top">

* [Enums.](#enums)
  * [Enum `switch`.](#enum-switch)
* [Choices.](#choices)
  * [Initialize choice.](#initialize-choice)
  * [Choice `switch`.](#choice-switch)
  * [Unwrap choice.](#unwrap-choice)
* [Traits.](#traits)
  * [Dynamic dispatch.](#dynamic-dispatch)
* [Type evaluation.](#type-evaluation)
* [Type templates.](#type-templates)
  * [`const` template parameter.](#const-template-parameter)
  * [Template specialization.](#template-specialization)
</td>
</tr></table>

[^top](#table-of-contents)

## Structs.
A struct type contains typed fields.

Struct types are declared with `type` followed by an optional `struct` keyword,
The following two declarations are equivalent:
```cy
type Vec2 struct:
    x float
    y float

type Vec2:
    x float
    y float
```

### Initialize struct.
Structs are constructed from an initializer expression:
```cy
v := Vec2{x=30, y=40}
print(v.x)      --> 30
```

An initializer literal can infer the target struct type:
```cy
var v Vec2 = {x=30, y=40}
```

Structs by default are copyable (unless a child member is not):
```cy
v := Vec2{x=30, y=40}
w := v
v.x = 100
print(w.x)     --> 30
print(v.x)     --> 100
```

### Default field values.
Struct initialization requires all fields to be specified, unless a default value was declared. Default field values must be **const expressions** :
```cy
type Vec2:
    x float = 0
    y float = 0

v := Vec2{}
print(v.x)       --> 0
print(v.y)       --> 0
```

Unlike a `struct`, a `cstruct` can default to their [zero values](#zero-values).

### Field visibility.
Fields have public visibility by default. However, when a field is declared with a `-` prefix, the field can only be accessed within the same module (although metaprogramming can get around this constraint):
```cy
type Info:
    a       int
    -b      int
    -secret str
```

### Circular references.
Field declarations may have circular type references if the struct can be initialized:
```cy
type Node:
    val  int
    next ?^Node

n := Node{val=123, next=none}
```
In the above example, `next` has an optional `?^Node` reference type so it can be initialized to `none` when creating a new `Node` instance.

The following `Node` type reports an error because it can not be initialized:
```cy
type Node:
    val  int
    next Node     --> CompileError. Circular reference.
```

### Type embedding.
Type embedding facilitates type composition by using the namespace of a child field's type: *Planned Feature*

```cy
type Base:
    a int

fn (&Base) double() -> int:
    return self.a * 2

type Container:
    b use Base

c := Container{b = Base{a=123}}
print(c.a)
--> 123
print(c.double())
--> 246
```

Note that embedding a type does not declare extra fields or methods in the containing type. It simply augments the type's using namespace by binding the embedding field.

If there is a member name conflict, the containing type's member has a higher precedence:

```cy
type Container:
    a int
    b use Base

c := Container{a=999, b = Base{a=123}}
print(c.a)
--> 999
print(c.double())
--> 246
```

Since the embedding field is named, it can be used just like any other field:
```cy
print(c.b.a)
--> 123
```

## `@init`.
Types can declare an `@init` function that gets invoked when calling the type as a function:
```cy
type Vec2:
    x float
    y float

fn Vec2 :: @init(x int, y int) -> Self:
    return Vec2{x=x, y=y}

v := Vec2(1, 2)
```

### `@init_sequence`.
`@init_sequence` overrides the sequence literal `T{_, _, ...}` which can contain a varying number of elements:
```cy
type MyArray

fn MyArray :: @init_sequence(init [&]int):
    print(init.len())
    return meta.init_type(MyArray, {})

arr := MyArray{1, 2, 3}
--> 3
```

### `@init_record`.
`@init_record` overrides the record literal `T{_=_, _=_, ...}` which can contain a varying number of record pairs:
```cy
type MyMap

fn MyMap :: @init_record(init [&]Pair[str, int]) -> Self:
    print(init.len())
    return meta.init_type(MyMap, {})

map := MyMap{a=123, b=234, c=345}
--> 3
```

## Methods.
Methods are functions that are invoked with a parent value (receiver).

They are declared by specifying the receiver's type before the function name and the other parameters. Inside a method body, `self` references the receiver's members as well as invoking other methods:
```cy
type Node:
    value int
    next  ?^Node

fn (&Node) inc(n int):
    self.value += n
    self.inc_one()

fn (&Node) inc_one():
    self.value += 1

var n = Node{value=123, next=none}
n.inc(321)    --> 445
```

The receiver type can be passed by value, reference, borrow, or pointer:
```cy
-- Pass by value.
fn (Node) inc(n int)

-- Pass by borrow.
fn (&Node) inc(n int)

-- Pass by exclusive borrow.
fn (&&Node) inc(n int)

-- Pass by reference.
fn (^Node) inc(n int)

-- Pass by pointer.
fn (Ptr[Node]) inc(n int)
```
It's recommended to use a borrow for the receiver unless there is a good reason not to.

### Operator methods.
Most operators are implemented as type methods.

The following is a list of operators:

| Operator | Name |
| --- | --- |
| Bitwise not, xor | `~` |
| Minus, Subtract  | `-` |
| Greater | `>` |
| Greater equal | `>=` |
| Less | `<` |
| Less equal | `<=` |
| Add | `+` |
| Multiply | `*` |
| Divide | `/` |
| Modulus | `%` |
| Power | `**` |
| Bitwise and | `&&` |
| Bitwise or | `||` |
| Bitwise left shift | `<<` |
| Bitwise right shift | `>>` |
| Address of index | `@index_addr` |
| Index | `@index` |
| Set index | `@set_index` |
| Slice | `@slice` |

Prefix operators only have a receiver parameter while infix operators have a receiver and RHS parameter. Currently, postfix operators cannot be overloaded. Since operator characters aren't allowed as standard identifiers, they are wrapped as raw string literals:
```cy
type Vec2:
    x float
    y float

fn (&Vec2) `+`(o Vec2) -> Vec2:
    return {
        x = self.x + o.x,
        y = self.y + o.y,
    }

fn (&Vec2) `-`() -> Vec2:
    return {x=-self.x, y=-self.y}

a := Vec2{x=1, y=2}
b := a + Vec2{x=3, y=4}
c := -a
```

Special operators have their own name. This example overloads the `index` operator and the `set index` operator:
```cy
type MyCollection:
    arr []int

fn (&MyCollection) @index(idx int) -> int:
    return self.arr[idx * 2]

fn (&MyCollection) @set_index(idx int, value int):
    self.arr[idx * 2] = val 

a := MyCollection{arr={1, 2, 3, 4}}
print(a[1])
--> 3
```

### Special methods.

The `@get` method allows overriding field accesses for undeclared fields:
```cy
type Foo

fn (&Foo) @get(%name EvalStr):
    return name.len()

f := Foo{}
print(f.abc)
--> 3

print(f.hello)
--> 5
```

The `@set` method allows overriding field assignments for undeclared fields:
```cy
type Foo

fn (&Foo) @set(%name EvalStr, value int):
    print('setting %{name} %{value}')

f := Foo{}
f.abc = 123
--> setting abc 123
```

## Tuples.
Tuples are declared using parentheses to wrap member fields:
```cy
type Vec2 struct(x float, y float)

-- Shorthand declaration.
type Vec(x float, y float)
```

If the fields share the same type, they can be declared in a field group:
```cy
type Vec3(x, y, z float)
```

Function and methods can still be declared inside the type's namespace:
```cy
type Vec2(x float, y float)

fn (&Vec2) scale(s float):
    self.x *= s
    self.y *= s
```

Tuples can be initialized with member values corresponding to the order they were declared:
```cy
v := Vec2{3, 4}
```

The initializer literal can infer the target tuple type:
```cy
var v Vec2 = {3, 4}
```

Tuples can still be initialized with explicit field names:
```cy
v := Vec2{x=3, y=4}
```

## Type namespace.
Functions and other symbols (except types) can be declared within the type's namespace.
`Self` is alias for the parent type:
```cy
type Node:
    value int
    next  ?^Node

fn Node :: @init() -> Self:
    return Node{value=123, next=none}

const Node :: DefaultValue = 100

n := Node()
print(n.value)             --> 123
print(Node.DefaultValue)   --> 100
```

## Type aliases.
A type alias refers to a different type.
Once declared, the alias and the target type can be used interchangeably:
```cy
type Vec2:
    x float
    y float

type Pos2 = Vec2

pos := Pos2{x=3, y=4}
```

## Enums.
An enum type is an exhaustive type where all possible values are defined by case members.
```cy
type Fruit enum:
    case apple
    case orange
    case banana
    case kiwi

fruit := Fruit.kiwi
print(fruit)       --> Fruit.kiwi
print(int(fruit))  --> 3
```

The memory representation of an enum defaults to `int`. Each case has an increasing value starting from 0.

A dot literal can infer a target enum type:
```cy
fruit := Fruit.kiwi
fruit = .orange
print(fruit == Fruit.orange)   --> true
```

### Enum `switch`.
`switch case` can match enum cases:
```cy
fn binary_search(arr []int, needle int, compare CompareFn) -> ?int:
    low := 0
    high := len
    while low < high:
        mid := low + (high - low) / 2
        switch compare(needle, mid):
            case .eq: return mid
            case .gt: low = mid + 1
            case .lt: high = mid
    return none
```

## Choices.
A choice type is an exhaustive type where only one defined case can be active. Each case member may contain a payload of an arbitrary type. An enum declaration becomes a choice declaration if one of the cases has a payload type specifier:
```cy
type Shape enum:
    case rectangle Rectangle
    case circle    Circle
    case triangle  Triangle
    case line      float
    case point 

type Circle(radius float)
type Rectangle(width, height float)
type Triangle(base, height float)
```

### Initialize choice.
A choice can be initialized with the case payload as an argument:
```cy
rect := Rectangle{width=10, height=20}
s := Shape.rectangle(rect)

-- Alternatively.
s = Shape.rectangle({width=10, height=20})

s = Shape.line(20)
```

A choice without a payload is initialized like an enum case:
```cy
s = Shape.point
```

### Choice `switch`.
`switch case` can match choice cases and capture the payload:
```cy
switch s:
    case .rectangle |r|:
        print('%{r.width} %{r.height}')
    case .circle |c|:
        print(c.radius)
    case .triangle |t|:
        print('%{t.base} %{t.height}')
    case .line |len|:
        print(len)
    case .point:
        print('a point')
    else:
        print('Unsupported.')
```

### Unwrap choice.
A choice can be unwrapped with the `.!` operator. This will either return the payload or signals a thread panic if the expected case is not active:
```cy
s := Shape.line(20)
print(s.!line)     --> 20
```

## Traits.
A trait is a generic type that defines a common interface for implementing types:
```cy
type Shape trait:
    fn area() -> float
```

Types can be declared to implement a trait with the `with` keyword:
```cy
type Circle:
    with Shape
    radius float

fn (&Circle) area() -> float:
    return 3.14 * self.radius^2

type Rectangle:
    with Shape
    width  float
    height float

fn (&Rectangle) area() -> float:
    return self.width * self.height
```
A type that intends to implement a trait but does not satisfy the trait's interface results in a compile error.

### Dynamic dispatch.
Since traits are a generic type, they need to be wrapped in a `Dyn` container to be materialized into a dynamic dispatch value. Implementing types become assignable to a `Dyn` reference type:
```cy
var s ^Dyn[Shape] = ^Circle{radius=2}
print(s.area())       --> 12.57

s = ^Rectangle{width=4, height=5}
print(s.area())       --> 20
```

## Type evaluation.
Types can be declared from const evaluation:
```
type File const:
    if meta.system() == .windows:
        return WinFile
    else:
        return PosixFile
```
This can be used to specialize types based on compile-time values.

## Type templates.
Type declarations can include template parameters.
Unlike function parameters, template parameters accept types rather than values of types by default. The type provided is constrained by a **generic** type specifier. The `Any` generic type allows any type:
```cy
type MyContainer[T Any]:
    id    int
    value T

fn (&MyContainer[]) get() -> T:
    return self.value
```

When the type template is expanded, a variant of the type is generated:
```cy
a := MyContainer[str]{id=123, value='abc'}
print(a.get())     --> abc
```
Expanding the template with the same parameters returns the same generated type. In other words, the generated type is always memoized from the input parameters.

### `const` template parameter.
Template parameters can accept const values other than types with the `const` modifier:
```cy
type MyArray[T Any, const N int]
```

### Template specialization.
Wrapping [type evaluation](#type-evaluation) as a type template allows template specialization from template parameters:
```cy
type MyContainer[T Any] const:
    if T == int:
        return IntContainer
    else:
        return GenericContainer[T]

a := MyContainer[int]{1, 2, 3}
```

# Control flow.
<table><tr>
<td valign="top">

* [Branching.](#branching)
  * [`if` statement.](#if-statement)
  * [`if` expression.](#if-expression)
  * [`and` expression.](#and-expression)
  * [`or` expression.](#or-expression)
* [Iterations.](#iterations)
  * [Infinite `while`.](#infinite-while)
  * [Conditional `while`.](#conditional-while)
  * [`for` range.](#for-range)
  * [`for` each.](#for-each)
  * [`break` statement.](#break-statement)
  * [`continue` statement.](#continue-statement)
</td><td valign="top">

* [`switch` matching.](#switch-matching)
  * [`case` range.](#case-range)
  * [`case` fallthrough.](#case-fallthrough)
  * [`switch` expression.](#switch-expression)
* [`begin` block.](#begin-block)
* [Error branching.](#error-branching)
* [Deferred execution.](#deferred-execution)
</td>
</tr></table>

[^top](#table-of-contents)

## Branching.

### `if` statement.
The `if` and `else` statements branch execution depending on conditions. The `else` clause can contain a condition which is only evaluated if the previous if/else conditional evaluated to `false`:
```cy
a := 10
if a == 10:
    print('a is 10')
else a == 20:
    print('a is 20')
else:
    print('neither 10 nor 20')
```

### `if` expression.
An `if` expression evaluates to a value depending on the condition.
Unlike the `if` statement, the `if` expression can not contain `else` conditions:
```cy
x := 123
b := if (x) 1 else 0
```

### `and` expression.
`and` evaluates to `true` if both operands are `true`. Otherwise, it evaluates to `false`. If the left operand is `false`, the evaluation of the right operand is skipped:
```cy
true and true    --> true
true and false   --> false
false and true   --> false
false and false  --> false

a := 10
if a > 5 and a < 15:
    print('a is between 5 and 15')
```

### `or` expression.
`or` evaluates to `true` if at least one of the operands is `true`. Otherwise, it evaluates to `false`. If the left operand is `true`, the evaluation of the right operand is skipped:
```cy
true or true     --> true
true or false    --> true
false or true    --> true
false or false   --> false

a := 10
if a == 20 or a == 10: 
    print('a is 10 or 20')
```

## Iterations.

### Infinite `while`.
The `while` keyword starts an infinite loop which continues to run the code in the block until a `break` or `return` is reached:
```cy
count := 0
while:
    if count > 100:
        break
    count += 1
```

### Conditional `while`.
When the `while` clause contains a condition, the loop continues to run until the condition is evaluated to `false`:
```cy
running := true
count := 0
while running:
    if count > 100:
        running = false
    count += 1
```

### `for` range.
`for` loops can iterate over a range that starts at an `int` (inclusive) to a target `int` (exclusive):
```cy
for 0..4:
    performAction() 
```

The loop's counter variable can be captured:
```cy
for 0..100 |i|:
    print(i)   --> 0, 1, 2, ... , 99
```

When `..=` is used, the target `int` is inclusive:
```cy
for 0..=100 |i|:
    print(i)   --> 0, 1, 2, ... , 100
```

To decrement the counter instead, use either `..>` or `..>=`:
```cy
for 100..>=0 |i|:
    print(i)   --> 100, 99, 98, ... , 0
```

### `for` each.
The `for` clause can iterate over any type that implements the `Iterable` trait. An Iterable contains an `iterator()` method which returns a value that implements the `Iterator` trait. The for loop continually invokes the iterator's `next()` method until `none` is returned.

A `Slice` can be iterated. The element value returned from an iterator's `next()` can be captured in the `|_|` clause:
```cy
arr := {1, 2, 3, 4, 5}

for arr |n|:
    print(n)
```

Iterating a `Map` yields `MapEntry` values:
```cy
map := {a=123, b=234}

for map |entry|:
    print(entry.key)
    print(entry.value)
```

A counting index can be captured before the **each** variable. The count starts at 0 for the first value:
```cy
arr := {1, 2, 3, 4, 5}
for arr |i, val|:
    print('index %{i}, value %{val}')
```

### `break` statement.
The `break` statement exits the current parent loop prematurely:
```cy
for 0..10 |i|:
    if i == 4:
        break
    print(i)
```

### `continue` statement.
The `continue` statement skips the rest of the current loop iteration and resumes execution on the next iteration:
```cy
for 0..10 |i|:
    if i == 4:
        -- Skips printing `4`.
        continue
    print(i)
```

## `switch` matching.
The `switch` statement matches on a control expression and branches to a case from a matching condition. Multiple cases can be grouped together with a comma separator:
```cy
val := 1000
switch val:
    case 100:
        print('val is 100')
    case 200, 300:
        print('combined case')
    else:
        print('val is %{val}')
```
An `else` fallback case is branched to when no other cases were matched.
A switch statement requires an `else` case unless the type is [`exhaustive`](#enum-switch).

### `case` range.
Case ranges can be declared for integer types. Unlike other range clauses, a case range's last value is inclusive:
```cy
val := 50 
switch val:
    case 0..100:
        print('at or between 0 and 100')
    case 'a'..'z', 'A'..'Z', '0'..'9', '_':
        print('an identifier character')
    else:
        print('val is %{val}')
```

### `case` fallthrough.
When a case is declared without a body, it will fallthough to the next case:
```cy
val := 1000
switch val:
    case 100
    case 200
    case 1000
        print('handle case')
    else:
        print('val is %{val}')
```

### `switch` expression.
The result of a `switch` statement can be assigned to a variable. Each case must return an expression:
```cy
shu := switch pepper:
    case 'bell'     => 0
    case 'anaheim'  => 500
    case 'jalapeño' => 2000
    case 'serrano'  => 10000
    else => -1
```

## `begin` block.
A `begin` block executes its body statements within a new scope:
```cy
a := 123

begin:
    a := 234
    print(a)
    --> 234    

print(a)
--> 123
```

## Error branching.
See [Error handling](#error-handling).

## Deferred execution.
> _Planned Feature_

# Functions.

<table><tr>
<td valign="top">

* [Function declaration.](#function-declaration)
  * [Function overloading.](#function-overloading)
  * [Parameter groups.](#parameter-groups)
  * [Named parameters.](#named-parameters)
* [Function values.](#function-values)
  * [Lambdas.](#lambdas)
  * [Inferred lambdas.](#inferred-lambdas)
  * [Closures.](#closures)
  * [Function pointer types.](#function-pointer-types)
  * [Function union types.](#function-union-types)
</td><td valign="top">

* [Function calls.](#function-calls)
  * [No parameter calls.](#no-parameter-calls)
  * [Shorthand calls.](#shorthand-calls)
* [Function templates.](#function-templates)
  * [Generic functions.](#generic-functions)
  * [Infer parameter.](#infer-parameter)
</td>
</tr></table>

[^top](#table-of-contents)

## Function declaration.
Functions must be declared with a name:
```cy
use math

fn dist(x0, y0, x1, y1 float) -> float:
    dx := x0 - x1
    dy := y0 - y1
    return math.sqrt(dx**2 + dy**2)
```

Functions can not reference outside local variables unless it's a [lambda](#lambdas):
```cy
a := 1

fn foo():
    print(a)    --> error: Undeclared variable `a`.
```

Functions can only return one value. However, the value can be destructured: *Planned Feature*
```cy
use math

fn compute(rad float) -> [2]float:
    return {math.cos(rad), math.sin(rad)}

{x, y} := compute(pi)
```

### Function overloading.
Functions can be overloaded by their type signature:
```cy
fn foo() -> int:
    return 2 + 2

fn foo(n int) -> int:
    return 10 + n

fn foo(n, m int) -> int:
    return n * m

print(foo())         --> 4
print(foo(2))        --> 12
print(foo(20, 5))    --> 100
```

## Parameter groups.
When multiple parameters share the same type they can be declared together in a sequence:
```cy
fn sum(a, b, c int) -> int
    return a + b + c
```

## Named parameters.
> _Planned Feature_

## Function values.
Functions can be assigned to variables or passed around as values:
```cy
-- Assigning to a local variable.
bar := dist

-- Passing `dist` as an argument.
print(sq_dist(dist, 30))

type DistFn = fn(float, float, float, float) -> float
fn sq_dist(dist DistFn, size float) -> float:
    return dist(0, 0, size, size) 

fn dist(x0, y0, x1, y1 float) -> float:
    dx := x0 - x1
    dy := y0 - y1
    return math.sqrt(dx**2 + dy**2)    
```

### Lambdas.
A lambda is an anonymous function that can only be referenced as a function value:
```cy
add := fn(a, b int) -> int:
    return a + b
```

### Inferred lambdas.
Inferred lambdas are declared with the capture `|_|` clause followed by the function body expression. The parameter and return types are inferred from the target function type:
```cy
-- Lambda without any parameters.
do(|_| print('hello'))

-- Lambda with a single parameter.
filter(|word| word.upper())

-- Lambda with multiple parameters.
symbol_name(|word, prefix| prefix + word.upper())

-- Assigning a lambda.
app.on_update = |delta_ms| update_physics(delta_ms)
```

A blockless statement can contain one lambda that has their function body continued in a new block:
```cy
queue_task(.high_priority, |_|):
    print('My important task.')
    do_stuff()
```

### Closures.
Lambdas can capture local variables with reference types from an immediate parent scope. The following example shows the lambda `f` capturing `a` from the main scope:
```cy
a := ^1
f := fn() -> int:
    return a.* + 2
print(f())     --> 3
```

When a closure captures a local variable that is not a reference `^T`, it becomes a pinned closure. A pinned closure cannot be copied or moved: *Still experimental*
```cy
a := 1
f := fn() -> int:
    return a + 2
print(f())     --> 3
```

### Function pointer types.
A function pointer type is denoted as `fn(P1, P2, ...) -> R` where `P`s are parameter types and `R` is the return type. Currently, function pointer types can only only be declared as a type alias:
```cy
type AddFn = fn(int, int) -> int
```

Function pointer types can include optional parameter names.
If one parameter has a name, the other parameters must also have names.
Parameter names do not alter the function signature and only serve as documentation:
```cy
type AddFn = fn(a int, b int) -> int
```

Functions and lambdas (excluding closures) can be assigned to a function pointer type:
```cy
fn add(a, b int) -> int:
    return a + b

type AddFn = fn(int, int) -> int

var func AddFn = add
func = |a, b| a + b + 123
```

### Function union types.
A function union type is denoted as `Func(FN)` where `FN` is a function pointer type.

It can hold closures in addition to functions and lambdas:
```cy
c := ^5
fn add_c(a int, b int) -> int:
    return a + b + c.*

type AddFn = fn(int, int) -> int

var func Func(AddFn) = add_c
func(10, 20)       --> 35
```

## Function calls.
Functions can be called with arguments wrapped in parentheses:
```cy
d := dist(100, 100, 200, 200)
```

Named arguments are required for named parameters:
> _Planned Feature_
```cy
d := dist(x0=10, x1=20, y0=30, y1=40)
```

### No parameter calls.
> _Planned Feature_

### Shorthand calls.
> _Planned Feature_

## Function templates.
Function declarations become function templates if they have template parameters:
```cy
fn add[T Any](a T, b T) -> T:
    return a + b
```

The function template can then be expanded to a function:
```cy
add_int := add[int]
print(add_int(1, 2))    --> 3
print(add[float](1, 2)) --> 3.0
```

### Generic functions.
When function's signature contains embedded template parameters `%`, it becomes a generic function:
```cy
fn add(%T type, a T, b T) -> T:
    return a + b
```

Generic functions automatically expand to a function when invoked.
The template parameter(s) are used to generate the appropriate function:
```cy
print(add(int, 1, 2))   --> 3
print(add(float, 1, 2)) --> 3.0
```

Note that invoking the function again with the same template parameter(s) uses the same generated function. The generated function is always memoized from the template parameters.

### Infer parameter.
When a template parameter is declared in a type specifier, it's inferred from the call  argument's type:
```cy
fn add(a %T, b T) -> T:
    return a + b

print(add(1, 2))        --> 3
print(add(1.0, 2.0))    --> 3.0
```

Nested template parameters can also be inferred:
```cy
fn set(m Map[%K, %V], key K, val V):
    m[key] = val
```

# Memory.

<table><tr>
<td valign="top">

* [Structured memory.](#structured-memory)
  * [Value ownership.](#value-ownership)
  * [Copy semantics.](#copy-semantics)
  * [Cloning.](#cloning)
  * [Moving.](#moving)
  * [Borrows.](#borrows)
  * [Exclusive borrows.](#exclusive-borrows)
  * [`self` borrow.](#self-borrow)
  * [`scope` parameter.](#scope-parameter)
  * [`sink` parameter.](#sink-parameter)
  * [Deinitializers.](#deinitializers)
</td><td valign="top">

* [Shared references.](#shared-references)
  * [ARC.](#arc)
  * [Retain optimizations.](#retain-optimizations)
  * [Default allocator.](#default-allocator)
  * [Weak references.](#weak-references)
  * [Cycle detection.](#cycle-detection)
* [Manual memory.](#manual-memory)
  * [Pointers.](#pointers)
  * [Memory allocations.](#memory-allocations)
</td>
</tr></table>

[^top](#table-of-contents)

Cyber provides memory safety by default by providing structured memory semantics.
Manual memory is also supported but it's forbidden in safe mode.

## Structured memory.
Cyber supports value ownership, borrowing, and deinitializers.
These concepts allow a value to be cleaned up automatically during execution because their lifetimes can be determined from analyzing the program's control flow.

This means that memory can be automatically managed without a garbage collector and deinitializer logic can be coupled with the value's lifetime.
This prevents memory and state bugs such as:
* Use before init, allocation.
* Use after deinit, invalidation, free.
* Invalid deinit, free. (decoupling value from its deinitializer logic)
* Free before allocated.
* Free with wrong allocator.
* Double free.
* Memory leaks. (forgetting to free)

At the same time, the use of structured memory is performant because it leverages cache locality and allows referencing to stack and interior heap members which flattens the memory hierarchy (less object indirection). In some cases, no-alias optimizations can be enabled because the compiler can prove there is only one reference pointing to a value. 

### Value ownership.
A value can be any type such as primitives, containers, and even references.

A value created on the stack can only have one owner; the variable it was assigned to, otherwise a temporary variable. A heap value can have multiple owners as shared references `^T`.

When the last owner goes out of scope (no longer reachable), its value is deinitialized.

At the end of a block, a child variable can no longer be accessed so its value is deinitialized:
```cy
a := 123

-- Deinit `a`.
```
In this case, it's effectively a no-op because `a` is a primitive `int` type.

If the value was a `str` (an immutable type backed by a reference counted buffer), the deinitialize logic would release a reference count (-1) on the underlying buffer:
```cy
a := 'hello'

-- Deinit `a`.
-- `a.buf` is released.
-- `a.buf` is freed.
```
Since the string buffer's reference count reaches 0, it's the last owner that points to the buffer so the buffer value is deinitialized (freed from heap memory).

The following initializes a value that holds a system resource:
```cy
a := os.open_file('foo.txt')!

-- Deinit `a`.
-- `a.@deinit()` is called.
-- `a.fd` file handle is closed.
```
In this case, `a` deinitializes by closing the file handle that it owns.

Owners can also go out of scope when they have been reassigned:
```cy
a := 123

-- Deinit `a`.
a = 234

b := Foo{a=123}

-- Deinit `b.a`.
b.a = 234
```

Value ownership allows the lifetime of values to be understood from the control flow. 
A value always knows **how** to deinitialize itself as described by its type, and the owner knows **when** to deinitialize the value.

### Copy semantics.
Commonly used data types are copyable such as primitives, `str`, `Slice`, and others.
A copyable type is implicitly copied. This can result in a shallow or deep copy depending on how the type is defined:
```cy
a := 123

b := a
-- Bit copy of `int`
```

Composite types are also copyable if all its members are copyable:
```cy
?int       -- Copyable option.

type Foo:  -- Copyable struct.
    a int
    b str
```

A custom type can also be copyable if it declares a `@copy` method:
```cy
type MyArray:
    ptr Ptr[byte]
    len int

-- Performs deep copy.
fn (&MyArray) @copy() -> Self:
    new_ptr := alloc(byte, self.len)
    @memcpy(new_ptr, self.ptr, self.len)
    return {ptr=new_ptr}
```

A type that implements the `NoCopy` trait prevents implicit copying:
```cy
type Foo:
    with NoCopy

    a int

f := Foo{a=123}

g := f
--> error: `Foo` is not copyable.
```

Some types such as `Array` disallows copying but can still be [cloned](#cloning) or [moved](#moving).

### Cloning.
Some value types cannot be implicitly copied but can be explicitly cloned: *Planned feature*
```cy
a := Array[int]{1, 2, 3}
b := clone(a)
```

Any implicitly `Copyable` type is also implicitly `Cloneable`. By default, `clone` will invoke the type's copy constructor:
```cy
a := 123

b := clone(a)
-- Equivalent to a copy.
```

The default `clone` behavior can be overridden by declaring a `@clone` method on the type:
```cy
type Foo:
    a int

fn (&Foo) @clone() -> Self:
    return {a=self.a}
```

### Moving.
Values can be moved, thereby transfering ownership from one variable to another:
```cy
a := 123
b := move a

print(a)
--> error: `a` does not own a value.
```

Some types such as `Array` can not be passed around by default without moving (or cloning) the value:
```cy
a := Array[int]{1, 2, 3}

print(compute_sum(move a))
```
In this case, the `Array` value is moved into the `compute_sum` function, so the `Array` is deinitialized by the callee function and not the callsite.

Values can be partially moved if a subset of its members were moved:
```cy
type Foo:
    a int
    b str

f := Foo{a=123, b='abc'}
b := move f.b
```

### Borrows.
Borrows are safe references to values that can never escape the stack.
Unlike unsafe pointers, a borrow is never concerned with when to free or deinitialize a value since that responsibility always belongs to the value's owner.
A borrow is guaranteed to point to an active value because they never outlive the lifetime of the active value.

Borrows grant **mutability** or read/write access to a value. 
**Multiple** borrows can be alive at once as long as there is no exclusive borrow alive at the same time.

A borrow type is denoted as `&T` where `T` is the type that the borrow points to.
The `&` operator is used to obtain a borrow to a value:
```cy
a := 123
ref := &a
ref.* = 234

print(a)
--> 234

inc(&a)
print(a)
--> 235

fn inc(a &int):
    a.* = a.* + 1
```

A borrow can not outlive the value it's referencing:
```cy
a := 123
ref := &a
if true:
    b := 234
    ref = &b
    --> error: `ref` can not outlive `b`.
```

Some dynamic data structure types allow borrowing a reference to an inner element:
```cy
a := Array[int]{1, 2, 3}
elem := &a[2]
elem.* = 300

print(a)
--> {1, 2, 300}
```
The element that `elem` points to can be mutated because the `Array` guarantees that the address remains stable.

### Exclusive borrows.
Like borrows, an exclusive borrow also grants **mutability** to a value.
A **single** exclusive borrow can be alive as long as no other borrows are also alive.
Exclusivity can be a useful constraint when invalidating an indirect buffer or value.
Since no other borrows are allowed to be alive at the same time, no borrows can become invalidated. Exclusivity also enables no-alias optimizations.

The `&&` prefix operator is used to obtain an exclusive borrow to a value.
An exclusive borrow type is denoted as `&&T` where `T` is the type that the reference points to.

`Array` is a type that requires an exclusive borrow for operations that can resize or reallocate its dynamic buffer:
```cy
a := Array[int]{1, 2, 3}
invalidate(&&a)

fn invalidate(arr &&Array[int]):
    a << 4
    if arr.len() > 3:
        arr.clear()
```
Note that invoking the append `<<` and `clear` methods automatically obtain an exclusive borrow for `self` without an explicit `&&` operator.

Resize operations such as `<<` and `clear` require an exclusive borrow because they can potentially reallocate a dynamic buffer, thereby invalidating other borrows.
If another borrow is alive before invoking these methods, the compiler would attempt to prematurely end the lifetime of the other borrows in order to satisfy the exclusivity constraint.
If this is not possible, then obtaining an exclusive borrow would result in a compile error:
```cy
a := Array[int]{1, 2, 3}
elem := &a[2]
a << 4
-- Ok. `elem` is no longer alive.

append_to(&a[2], &&a)
--> error: Can not obtain exclusive borrow, `&a[2]` is still alive.

fn append_to(elem &int, arr &&Array[int]):
    arr << elem.*
```

### `self` borrow.
Methods can be declared to accept borrows or exclusive borrows from the `self` receiver:
```cy
type Foo:
    a int

fn (&Foo) mutate():
    self.a = 123
```

Invoking methods automatically attempts to obtain the correct borrow type as specified by the method:
```cy
f := Foo{a=1}
f.mutate()
-- Obtain borrow to `f`.
```

### `scope` parameter.
Functions can only return borrows if the scope is bound to a borrow parameter. This is possible with the `scope` modifier:
```cy
fn (scope &FooArray) @index_addr(idx int) -> scope &Foo:
    return &self.inner[idx]
```
The `scope` binding informs the callsite that the returned borrow belongs to the same scope as a borrowed argument. This ensures the returned borrow's lifetime does not exceed the lifetime of the borrowed argument.

Any type that contains a borrow member becomes a borrow container and requires the `scope` binding:
```cy
type FooIterator:
    rec &FooArray
    idx int

fn (scope &FooArray) iterator() -> scope FooIterator:
    return {
        rec = self,
        idx = 0,
    }
```

### `sink` parameter.
The `sink` modifier accepts and consumes a value argument: *This is not much different from a `move` operation, so this feature may be removed.*
```cy
fn (sink Array[]) as_buffer() -> Buffer[T]:
    buf := self.buf
    length := self.length
    cap := self.cap()
    @consume(self)
    return {
        base   = buf,
        length = length,
        header = cap,
    }
```

### Deinitializers.
When a value is no longer alive, its deinitializer is invoked.
Under normal conditions, the procedure follows these steps:
1. The value's custom `@deinit` method is invoked.
2. Performs the deinitialize procedure for any child values (defined as type members).

Custom deinitializers can be declared with a `@deinit` method:
```cy
type File:
    fd int

fn (&File) @deinit():
    C.close(self.fd)
```

On thread panic, the runtime begins its **fatal** deinitialization procedure.
Value deinitializers are only invoked for types that implement `Unwind`. *TBD*
In addition, the deinitializers are not invoked in any hierarchical order but rather a flattened order (as the runtime iterates the alive values at the time of the panic).

## Shared references.
Shared references `^T` point to objects (values that were allocated on the heap).
The usage of shared references has been described in [Basic types -> References](#references). This section describes their implementation details and how the runtime manages them.

### ARC.
Objects in Cyber are reference counted. Each shared reference retains the count upon initialization and releases the count when it goes out of scope. The value that the reference points to is deinitialized and freed when the reference count reaches zero. This is also known as ARC (automatic reference counting).

Shared references are meant to describe a shared dependency to an object when it would be inconvenient to do so through single value ownership. It is not meant to describe a cyclic graph of objects (where there is no clear ownership hierarchy). In fact, reference cycles is a sign of a bug in the program and is [reported](#cycle-detection).

### Retain optimizations.
Reference counting is not zero-cost. Retaining and releasing an object's reference count is a write operation so the compiler will look for opportunities to keep the book keeping to a minimum.
It turns out a significant amount of retain/release ops can be avoided when the lifetime of a reference is known on the stack.
For example, passing a reference to a function call doesn't need a retain since it is guaranteed to be alive when the call returns.

### Default allocator.
Currently, `mimalloc` is used as the default heap allocator for objects, but it can be swapped with libc `malloc` or a custom allocator.

### Weak references.
> _TBD_

### Cycle detection.
When shared references form a reference cycle, it's considered a runtime error.
In safe mode, cycle detection is dispatched at the end of a thread.
The thread will panic if a cycle is detected which subsequently begins the runtime's [fatal deinitialization procedure](#deinitializers).

When enabled, the cycle detector can also be invoked manually `@check_cycles`: *TBD*
```cy
fn foo():
    -- Create a reference cycle.
    a := ^Foo{child=none}
    b := ^Foo{child=none}
    a.child = b
    b.child = a

    -- Cycle still alive in the current stack so it reports no cyclic objects.
    res := @check_cycles()
    print(res.num_cyc_objs)   --> 0

foo()
-- `a` and `b` are no longer reachable.
res := @check_cycles()
print(res.num_cyc_objs)       --> 2
```

## Manual memory.
> _Planned Feature_

### Pointers.
See [FFI / Pointers](#pointers-1).

### Memory allocations.
> _Planned Feature_

# Error handling.
<table><tr>
<td valign="top">

* [Errors.](#errors)
  * [`error` literal.](#error-literal)
  * [`error` payload.](#error-payload)
  * [`error` set type.](#error-set-type)
* [Results.](#results)
  * [Unwrap or rethrow.](#unwrap-or-rethrow)
  * [Unwrap or default.](#unwrap-or-default)
  * [Unwrap or guard.](#unwrap-or-guard)
  * [Unwrap block.](#unwrap-block)
</td><td valign="top">

* [Panics.](#panics)
* [Stack traces.](#stack-traces)
</td>
</tr></table>

[^top](#table-of-contents)

## Errors.
An `error` is copyable value that should be handled at the call site or bubbled up.

### `error` literal.
An error value can be constructed from an `error` literal:
```cy
err := error.Oops
```

`error` values can be compared using the `==` operator:
```cy
if err == error.Oops:
    handle_oops()
```

### `error` payload.
A payload value can be attached when creating an error value. *TBD*

### `error` set type.
An error set type is an exhaustive type of possible error values: *TBD*
```cy
type MyError error:
    case Boom
    case BadArgument
    case NameTooLong

err := MyError.NameTooLong
```

## Results.
A result value forces the call site to unwrap it in order to use the payload.

A result type is a choice type that holds either an `error` or a payload.
It is denoted as `!T` where `T` is the payload type.

It can be constructed by inferring an error or a payload value:
```cy
var res !str = error.Failed

res = 'abcxyz'
```

In practice, results are typically constructed when returning from a function:
```cy
fn compute(input int) -> !int:
    if input == 42:
        return error.WhyPick42

    return fib(input)
```

A function that can fail but has no payload would return `!void`:
```cy
fn validate(name str) -> !void:
    if name.len() > 64:
        return error.NameTooLong
```

### Unwrap or rethrow.
The `!` postfix operator unwraps a result's payload or rethrows the error:
```cy
data := os.read_file('data.txt')!
```
In the main block, the rethrow case would result in a thread panic.

If the unwrap expression is inside a function, the rethrow case would bubble up the error to the caller. This suggests that the function must have a `Result` return type:
```cy
fn content_length(path str) -> !int:
    data := os.read_file(path)!
    return data.len()
```

### Unwrap or default.
The `!else` expression either unwraps a result's payload or defaults to a value:
```cy
res := do_something() !else 0
```

The default case can be implemented in a block:
```cy
res := do_something() !else:
    panic('Failed.')
```

The error value can be captured:
```cy
res := do_something() !else |err|:
    panic('Failed with %{err}')
```

The `try` block catches thrown errors and resumes execution in a followup `catch` block:
```cy
try:
    funcThatCanFail()
catch err:
    print err      -- 'error.Failed'
```

### Unwrap or guard.
*TBD*

### Unwrap block.
Sometimes it can be useful to catch all errors thrown in a block: *TBD*
```cy
try:
    res := do_something()!
    do_even_more_things(res)!
    validate(res)!
    print('Success.')
else |err|:
    panic('Failed with %{err}.')
```

## Panics.
The builtin `panic` is a fail-fast mechanism to quickly exit the current thread with an error message:
```cy
panic('oops')
```
Panics can not be caught. Once `panic` is invoked, the current thread stops execution and begins to unwind its call stack. Afterwards, the thread can not be used and transitions to a panic state.
If the main thread panics, then the program aborts without unwinding other child threads.

A parent thread can obtain the status of a child thread. *TBD*

A panic reports the [stack trace](#stack-trace) automatically to the console.

## Stack traces.
The builtin `stack_trace()` and `stack_trace_info()` are used to obtain the stack trace info at any point in the program:
```cy
fn some_nested_func():
    -- Prints the stack trace summary.
    print(stack_trace())

    -- Provides structured info about the stack trace.
    info := stack_trace_info()
    print(info.frames.len())
```

# Concurrency.

<table><tr>
<td valign="top">

* [Threads.](#threads)
  * [Spawn threads.](#spawn-threads)
  * [`Sendable` values.](#sendable-values)
  * [Fibers.](#fibers)
  * [Growable stacks.](#growable-stacks)
  * [Gas mileage.](#gas-mileage)
* [OS threads.](#os-threads)
* [SIMD.](#simd)
</td><td valign="top">

* [Futures.](#futures)
  * [Future `await`.](#future-await)
  * [Future chains.](#future-chains)
  * [Resolving futures.](#resolving-futures)
* [Shared memory.](#shared-memory)
  * [Channels.](#channels)
* [`await` union.](#await-union)
* [Generators.](#generators)
</td>
</tr></table>

[^top](#table-of-contents)

## Threads.
Threads are light-weight virtual threads that have their own execution context and heap memory. The program can spawn many threads but only some of them can run in parallel depending on the operating system and the number of CPU cores.

Threads are isolated from one another and can communicate by message passing or by sharing memory safely.

### Spawn threads.
The builtin `spawn` creates and starts a new thread with a function as the entry point:
```cy
task := spawn(fib, {40})

fn fib(n int) -> int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)
```

Similarly, a thread can be spawned with a lambda as the entry point: *TBD*
```cy
task := spawn(|_| -> int, {}):
    -- Do computation.
```

`spawn` returns a `Result[Future[T]]` where `T` is the return type of the entry function. If the runtime could not dispatch the thread, a panic is raised.
If the thread was dispatched, the wrapped `Future` contains the asynchronous result of the new thread.

### `Sendable` values.
Values that can be transferred from one thread to another must be types that implement `Sendable`.

Here is a list of common types that implement `Sendable`:
* Primitives types: `int`, `byte`, `bool`
* `str`
* `?T` if `T` is `Sendable`
* `!T` if `T` is `Sendable`
* `[]T` if `T` is `Sendable`. `[]T` is cloned when sending.
* `Array[T]` if `T`  is `Sendable`
* `Map[K, V]` if `K` and `V` are `Sendable`
* `[N]T` if `T` is `Sendable`
* `[..N]T` if `T` is `Sendable`
* Composite types if all member types are `Sendable`.
* Choice types if all member payload types are `Sendable`.
* Function pointers.
* `Ptr[T]`.

Types that don't implement `Sendable` include:
* Reference types: `^T`, `&T`, `&&T`, `[&]T`.
* Function unions. (Closures are not `Sendable`)

### Fibers.
A fiber is a cooperative execution context that belongs to a parent thread.
It has its own stack but shares the thread's heap: *TODO*
```cy
f := Fiber(|_|):
    print('in a fiber')

f.resume()
--> in a fiber
```
When a fiber panics, all other fibers in the same thread are terminated.

Fibers are cooperative using a resume and yield mechanism:
```cy
fn two_steps():
    print('first')
    Fiber.yield()
    print('last')

f := Fiber(two_steps, {})
f.resume()
--> first

f.resume()
--> last
```

When a fiber is destructed, it will panic if it's still in progress:
```cy
fn forever():
    while:
        Fiber.yield()

f := Fiber(forever, {})
f.resume()

--> panic: Fiber is still in progress.
```

### Growable stacks.
When running on Cyber's VM, threads can grow their stack on demand.
The compiler generates pointer layouts that tells the runtime where pointer values are so they can be patched with new addresses.

Growable stacks allow threads to be lighter. They can be initialized with a smaller stack and grow on demand. However, this feature is not available when compiling AOT where stacks are fixed in size.

### Gas mileage.
*TBD*

## OS threads.
*TBD*

## SIMD.
*TBD*

## Futures.
A `Future` is an asynchronous result type. It represents the result of work that will either complete or fail at some point in the future.
This abstraction allows the current thread to continue execution without waiting for the completion of the `Future`.

Futures can represent asynchronous work that is run in parallel or on a single thread.
I/O bound work can be delegated to the operating system and CPU bound work can be run across multiple threads. A `Future` is not concerned with how the work is accomplished.

If an API function is meant to do work asynchronously, it would return a `Future`:
```cy
use aio

work := aio.delay(1000)
print(work)
--> Future[void]
```

Futures can hold a result value when they are completed:
```cy
use aio

work := aio.read_file_defer('foo.txt')
print(work)
--> Future[[]byte]
```

Futures can be created with a completed value:
```cy
work := Future.complete(100)
print(f)
--> Future[int]

print(f.get().?)
--> 100
```

### Future `await`.
`Future.await()` asynchronously waits for a `Future` to complete.

`await` suspends the current thread so that the scheduler can resume other threads that are in a ready state.

When `await` resumes, the expression evaluates to the completed value of the `Future`:
```cy
use aio

work := aio.read_file_defer('foo.txt')
print(work.await())
--> "foo.txt contents"
```

`await` can be used in any function which makes asynchronous functions **colorless**. They do not need a special function modifier. This means that asynchronous work can be wrapped in a synchronous API.

### Future chains.
`Future.then_spawn` spawns another thread that is when the future completes, thereby creating an asynchronous chain. The runtime prefers to reuse the same worker that completed the future when possible: *TBD*
```cy
use aio

res := aio.read_file_defer('foo.txt').then_spawn(|contents|):
    print(contents)
    --> "foo.txt contents"

print(res)
--> Future[void]
```

Like `spawn`, the continuation can return a value:
```cy
res := aio.read_file_defer('foo.txt').then_spawn(|contents| -> int):
    return contents.len()

print(res)
--> Future[int]

print(res.await())
--> 3
```

### Resolving futures. 
A `Future` can be produced and completed by a `FutureResolver`: *TBD*
```cy
r := FutureResolver(int)
f := r.future()

spawn(|r Resolver|, {r}):
    r.complete(234)

v := f.await()
print(v)
--> 234
```

## Shared memory.
*TBD*

### Channels.
*TBD*

## `await` union.
*TBD*

## Generators.
Generators are stackless coroutines that are intended to yield values incrementally.
A function annotated with `#[generator]` returns a `Generator[Fn]` when invoked:
```
#[generator]
fn iterate() -> int:
    yield 123
    yield 456

gen := iterate()
print(gen)
--> Generator[fn()->int]
```
`yield` pauses the current generator and returns a value back to the call site of `Generator.next`.

A generator begins and resumes execution with `next` which returns the next yielded value as an optional:
```cy
while gen.next() |res|:
    print(res)
    --> 123
    --> 456
```

The current state of a generator can be obtained with `Generator.status`:
```cy
gen := iterate()
print(gen.status())
--> GeneratorStatus.paused

while gen.next() |res|:
    pass

print(gen.status())
--> GeneratorStatus.done
```

A generator can be reset to its entry point with the original arguments or different arguments: *TBD*

# Metaprogramming.

<table><tr>
<td valign="top">

* [Compile-time execution.](#compile-time-execution)
  * [Inline evaluation.](#inline-evaluation)
  * [Inline type creation.](#inline-type-creation)
  * [`#if` inline.](#if-inline)
  * [`#for` inline.](#for-inline)
  * [`#switch` inline.](#switch-inline)
  * [`switch #for` inline.](#switch-for-inline)
  * [Compile-time variables.](#compile-time-variables)
* [Conditional compilation.](#conditional-compilation)
* [Runtime execution.](#runtime-execution)
</td><td valign="top">

* [`meta` module](#meta-module)
* [Reflection.](#reflection)
* [`#bind` hooks.](#bind-hooks)
* [Templates.](#templates)
* [Generic types.](#generic-types)
* [Compile-time types.](#compile-time-types)
  * [`type` type.](#type-type)
  * [`fnsym` type.](#fnsym-type)
  * [`EvalStr` type.](#evalstr-type)
  * [`EvalInt` type.](#evalint-type)
* [Attributes.](#attributes)
</td>
</tr></table>

[^top](#table-of-contents)

## Compile-time execution.
Compile-time execution is run during the compilation of the user's program.

### Inline evaluation.
Inline evaluation is a form of compile-time execution where the code is evaluated as each node in the AST is visited. This can not emulate the entire language but many basic operations can be evaluated and eligible types can be materialized into IR for code generation.

A `const` declaration evaluates its initializer at compile-time which also evaluates the recursive calls to the `fib` function:
```cy
fn fib(n int) -> int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

const fib30 = fib(30)
```

An expression needs to be wrapped with `#{}` to perform inline evaluation if it's not already in a compile-time context:
```cy
-- Assign pre-computed value to a local.
res := #{fib(30)}
```

### Inline type creation.
Types can be created from inline evaluation. Currently, only structs can be created with `meta.new_struct`:
```cy
type MyType[T Any] const: 
    struct_t := type.info(T).!struct
    fields := [struct_t.fields.length]meta.StructField({name='', type=void, offset=0, state_offset=0})
    for 0..struct_t.fields.length |i|:
        field := struct_t.fields[i]
        fields[i] = {
            name = 'myfield' + str(i),
            type = field.type,
            offset = 0,
            state_offset = 0,
        }
    return meta.new_struct(fields, {})
```

### `#if` inline.
The `#if` statement performs inline evaluation on the condition expression and inserts it's child statements if the condition evaluates to `true`:
```cy
#if meta.system() == .windows:
    print('Running on Windows.')
#else:
    print('Running on %{meta.system()}')
```

### `#for` inline.
The `#for` statement iterates at compile-time and inserts it's child statements on every iteration. The following prints the name of every field in the type `Foo`:
```cy
#struct_t := type.info(Foo).!struct
#for 0..struct_t.fields.length |i|:
    #field := struct_t.fields[i]
    print(#{field.name})
```

### `#switch` inline.
The `#switch` statement evaluates at compile-time and inserts the child statements of the matching case block:
```cy
#switch type.info(T):
    #case .struct |struct_t|:
        print('a struct')
    #case .option |option_t|:
        print('an optional')
    #else:
        print('unsupported')
```

### `switch #for` inline.
A regular `switch` statement with a `#for` block can automatically expand all `case` statements:
```cy
switch value:
    #for meta.enum_values(MyEnum) |Tag|:
        case Tag:
            print('value is a %{Tag}')
```

### Compile-time variables.
Compile-time variables are assigned with a leading `#`:
```cy
#info := type.info(Foo)
```

## Conditional compilation.
*TBD*

## Runtime execution.
The `cy` module provides an API to [libcyber](#libcyber).

`cy.eval` evaluates source code in an isolated VM:
```cy
use cy

res := cy.eval('''
fn main() -> int:
    return 1 + 2
''')!
print(res)
--> 3
```

## `meta` module.
The `meta` module contains metaprogramming utilities. See the [`meta` docs](api.html#meta).

## Reflection.
`type.info` returns compile-time type info. See [`TypeInfo` docs](api.html#meta):
```cy
#info := type.info(Foo).!struct
#meta.log(info.fields.length)
```

Runtime type info is currently limited to a name and ID:
```cy
rt_type := MetaType(Foo)
print(rt_type.id())
--> 123

print(rt_type.name())
--> Foo
```

## `#[bind]` hooks.
The `#[bind]` annotation creates a compiler hook for a type or function declaration.
[`libcyber`](#libcyber) allows an embedder to register these hooks.
Many of the builtins in Cyber core and std modules are implemented using these bind hooks.

## Templates.
Templates generate varying types and functions with template parameters.
See [Custom types / Type templates](#type-templates) and [Functions / Function templates](#function-templates).

## Generic types.
Generic types are type constraints for parametric polymorphism. They aren't concrete types themselves but rather a placeholder that constrains an input type. They are used in template parameters and generic function parameters.

The `Any` type is a generic type that has no constraints, allowing any `type`:
```cy
type MyContainer[T Any]:
    inner T
```

Traits are considered generic types and constrain types to those that implement its interface:
```cy
type Shape trait:
    fn area() -> float

fn less(a Shape, b Shape):
    return a.area() < b.area()
```
When a function parameter contains a generic type, the function becomes generic. The function is then specialized by the call site arguments.

Type templates are considered generic types: *TBD*
```cy
fn size(c MyContainer) -> int:
    return type.size(type.of(c))
```

Composing a generic type creates another generic type: *TBD*
```cy
fn size(c ?MyContainer) -> int:
    return type.size(type.of(c))
```

## Compile-time types.
Some types can only be used at compile-time.

### `type` type.
A `type` represents a compiler type symbol and only exists at compile-time.

### `fnsym` type.
An `fnsym` represents a compiler function symbol that can be expanded to a runtime function pointer or a function call. They can be used as parameterized values:
```cy
type IntMap[K Any, const HASH fnsym(K)->int]
    ptr [*]int
    len int

fn (&IntMap[]) get(key K) -> int:
    slot := HASH(key) % self.len
    return self.ptr[slot]

fn my_hash(s str) -> int:
    return s.len()

m := IntMap[str, my_hash]{}
```

### `EvalStr` type.
`EvalStr` can be expanded to a `str`, `[]byte`, zero terminated `Ptr[byte]`, `int`, or `byte`.

### `EvalInt` type.
`EvalInt` can be expanded to different integer types. Currently, it has the extent of an `int` type.

## Attributes.
*TBD*

# Modules.
<table><tr>
<td valign="top">

* [Main module.](#main-module)
* [Main function.](#main-function)
* [Builtin modules.](#builtin-modules)
* [Importing.](#importing)
  * [Import file.](#import-file)
  * [Import URL.](#import-url)
  * [Import all.](#import-all)
  * [Circular imports.](#circular-imports)
  * [Destructure import.](#destructure-import)
* [Exporting.](#exporting)
* [Symbol visibility.](#symbol-visibility)
* [Symbol alias.](#symbol-alias)

</td><td valign="top">

</td>
</tr></table>

[^top](#table-of-contents)

Modules have their own namespace of static symbols. By default, importing another Cyber script returns a module with its declared symbols.

## Main module.
The main module can contain top-level imperative statements if no main function is declared.
An imported module containing top-level statements returns an error:
```cy
-- main.cy
print('ok')

-- foo.cy
print('not ok')         
-- error: Top-level statement not allowed.
```

## Main function.
The main module can contain a main function. It's the starting point for a program:
```cy
fn main():
    print('program start')
```

## Builtin modules.
Builtin and std modules come with Cyber. See the [API docs](api.html).

## Importing.
The `use` statement can import modules. Builtin and std modules such as `math` can be imported without any configuration:
```cy
use math

print(math.cos(0))
```
When a `use` statement contains only a single identifier, it reuses the module name as the local name.

To bind to a different local name, the `use` statement requires a name before the import specifier. Note that this also requires the import specifier to be a string literal:
```cy
use m 'math'

print(m.random())
```

### Import file.
Source files can be imported from the local file system:
```cy
-- Importing a module from the local directory.
use b 'bar.cy'

print(b.myFunc())
print(b.myVar)
```

### Import URL.
Source files can be imported from the Internet:
```cy
-- Importing a module from a CDN.
use rl 'https://mycdn.com/raylib'
```

When importing a URL without a file name, Cyber's CLI will look for a `mod.cy` from the path instead.

### Import all.
If the alias name is the wildcard character, all symbols from the module are imported into the using namespace: *This feature is experimental and may be removed in a future version.*
```cy
use * 'math'

print(random())
```

### Circular imports.
Circular imports are allowed. In the following example, `main.cy` and `foo.cy` import each other without problems.
```cy
-- main.cy
use foo 'foo.cy'

fn print_b():
    foo.print_c()

foo.print_a()

-- foo.cy
use main 'main.cy'

fn print_a():
    main.print_b()

fn print_c():
    print('done')
```
However, this doesn't imply that circular symbol dependencies are allowed. Different symbols have their own rules for circular dependencies. 

### Destructure import.
Modules can also be destructured using the following syntax:
> _Planned Feature_
```cy
use { cos, pi } 'math'

print(cos(pi))
```

## Exporting.
All symbols are exported by default without any additional modifiers:
```cy
fn foo():          
    pass

global state int = 234

type Foo:
    a float
```

Any symbol alias declared from `use` is not exported:
```cy
-- Not visible from other modules.
use math
```

## Symbol visibility.
Symbols can have private visibility when declared with a `-` prefix.
This only allows the source module to access the symbol:
```cy
-type Foo:
    a int
    b int

-fn add(a, b int) -> int:
    return a + b
```

## Symbol alias.
`use` can create an alias to another symbol:
```cy
use eng 'lib/engine.cy'

use Vec2 -> eng.Vector2
```

# FFI.

<table><tr>
<td valign="top">

* [C primitives.](#c-primitives)
* [Pointers.](#pointers-1)
  * [Address of.](#address-of)
  * [Dereferencing pointers.](#dereferencing-pointers)
  * [Pointer indexing.](#pointer-indexing)
  * [Pointer arithmetic.](#pointer-arithmetic)
* [Pointer spans.](#pointer-spans)
* [C strings.](#c-strings)
* [C structs.](#c-structs)
* [C unions.](#c-unions)
* [Zero values.](#zero-values)
</td><td valign="top">

* [Binding to C.](#binding-to-c)
  * [Static binding.](#static-binding)
  * [Runtime binding.](#runtime-binding)
  * [`open_lib` binding.](#open_lib-binding)
  * [`extern` functions.](#extern-functions)
  * [`extern` variables.](#extern-globals)
  * [cbindgen.cy](#cbindgency)
</td>
</tr></table>

[^top](#table-of-contents)

## C primitives.
[mod c](api.html#c) contains aliases to C types that are compiler/architecture dependent. These can be used when generating bindings or interfacing with C.

This table maps Cyber to C primitive types:

| Cyber | C |
| --- | --- |
| `void` | `void` |
| `i8` | `int8_t` |
| `r8`, `byte` | `uint8_t` |
| `c_char` | `char` |
| `i16` | `int16_t` | 
| `r16` | `uint16_t` | 
| `c_short` | `short` |
| `c_ushort` | `unsigned short` | 
| `i32` | `int32_t` | 
| `r32` | `uint32_t` | 
| `c_int` | `int` |
| `c_uint` | `unsigned int` |
| `i64`, `int` | `int64_t` | 
| `r64` | `uint64_t` | 
| `c_long` | `long` |
| `c_ulong` | `unsigned long` | 
| `c_longlong` | `long long` | 
| `c_ulonglong` | `unsigned long long` | 
| `f32` | `float` |
| `f64`, `float` | `double` |
| `N/A` | `long double` | 
| `Ptr[T]` | `T*` |

## Pointers.
A `Ptr[T]` type is an unmanaged reference type to the address of a `T` value.
`none` evaluates to NULL or 0:
```cy
var ptr Ptr[int] = none
```

An address can be casted to a pointer type:
```cy
var ptr Ptr[int] = as 0xDEADBEEF
```

### Address of.
The `*` operator returns the address to a value as a pointer `Ptr[T]`:
```cy
value := 123
ptr := *value
ptr.* = 1000

print(value.x)
--> 1000
```

The right side of a `*` has higher precedence so it can refer to an inner member:
```cy
type Vec2 cstruct:
    x float
    y float

v := Vec2{}
ptr := *v.x
```

### Dereferencing pointers.
Pointers are dereferenced with the `.*` operator:
```cy
a := 123
ptr := *a
print(ptr.*)
--> 123

ptr.* = 10
print(a)
--> 10
```

Accessing a member automatically dereferences the parent pointer:
```cy
type Vec2 cstruct:
    x float
    y float

v := Vec2{x=30, y=40}
ptr := *v
print(ptr.x)   --> 30
```

### Pointer indexing.
The index operator can access the nth element:
```cy
arr := alloc(int, 10).ptr
arr[5] = 123
print(arr[5])
--> 123
```
Negative indexing will locate the element before the pointer's address.

Slicing returns a new span `PtrSpan[T]` over the given range:
```cy
span := arr[0..5]
```

### Pointer arithmetic.
Addition advances the address of `Ptr[T]` by the size of `T`:
```cy
arr := alloc(int, 10).ptr
arr += 1
(arr + 3).* = 234
print((arr + 3).*)    --> 234
```

Subtraction between two pointers of the same type returns the difference in units of `T`:
```cy
fn pc_off(base Ptr[Inst], pc Ptr[Inst]) -> int:
    return pc - base
```

## Pointer spans.
Pointer spans contains a pointer and a length. The type is denoted as `PtrSpan[T]` where `T` is the element type.

Read and write to the nth element with the index operator: 
```cy
span := alloc(int, 10)
print(span[0])
--> 1

span[1] = 123
print(span[1])
--> 123
```

A `PtrSpan` has bounds checking when runtime safety is enabled:
```cy
print(s[100])
--> panic: Out of bounds.
```

A `PtrSpan` can be sliced:
```cy
a := span[0..2]
```

## C strings.
There are utilities in the `c` module to convert between `str` and a zero terminated C string:
```cy
use c

s := c.from_strz('c string')
c_str := c.to_strz(s)
```

A `str` can also be constructed with zero appended as the last character. Since `str` is a managed type, it doesnt need an explicit free:
```cy
sz := str.initz('c string')
```

## C structs.
A `cstruct` type mimics the memory layout of a C struct type:
```cy
type Data cstruct:
    x   float
    y   float
    ptr Ptr[int]
```

A `cstruct` may contain:
* C structs/unions types.
* Primitive types.
* `enum` types.

## C unions.
A `cunion` type mimics the memory layout of a C union type:
```cy
type Data cunion:
    case i int
    case f float
    case s Foo
```

A `cunion` may contain:
* C structs/unions types.
* Primitive types.
* `enum` types.

A `cunion` type is constructed with a case wrapping its payload:
```cy
cdata := Data.i(123)

print(cdata.i)
--> 123
```

## Zero values.
Uninitialized C struct members default to their zero values:
```cy
type Vec2 cstruct:
    x float
    y float

v := Vec2{}
print(v.x)      --> 0
print(v.y)      --> 0
```

The following shows the zero values of C and C compatible types:

|Type|Zero value|
|---|---|
|`bool`|`false`|
|`i8`|`0`|
|`i16`|`0`|
|`i32`|`0`|
|`i64`, `int`|`0`|
|`r8`, `byte`|`0`|
|`r16`|`0`|
|`r32`|`0`|
|`r64`|`0`|
|`f32`|`0.0`|
|`f64`, `float`|`0.0`|
|`Ptr[T]`|`none`|
|`type T cstruct`|`{}`|
|`type T cunion`|`{}`|

## Binding to C.
Cyber supports binding to an existing C ABI compatible library at runtime.
This allows calling into dynamic libraries created in C or other languages.
When compiled AOT, the libraries can be linked statically.

There are different approaches to binding a C library:
1. Static binding. The application and C library is compiled into a single executable. 
2. Runtime binding. The C library is loaded and binded upon program start-up.
3. `os.open_lib` binding. The user is responsible for loading the C library at runtime.
4. When embedding [`libcyber`](#libcyber), compiler hooks can bind VM or C functions with the host language.

### Static binding.
When targeting the C backend, the C library's source or static library can be included for compilation. The final output will be a statically linked executable. *TODO*

`extern` functions and variables are required to link with the C library. They can be autogenerated with [`cbindgen.cy`](#cbindgency) and the library's header file.

Source code for the library can be declared with `c.source`:
```cy
use c

#c.source('mylib.c')
```

`cc` build flags can be declared with `c.flag`:
```cy
#if meta.system() == .macos:
    #c.flag('-Dmacos')
```

A static library can be linked with `c.static_lib`:
```cy
#c.static_lib('mylib.a')
```

### Runtime binding.
Cyber uses `libtcc` to JIT compile the bindings from `extern` declarations.
An example can be found in [ffi.cy](https://github.com/fubark/cyber/blob/master/examples/ffi.cy).

`#extern` bindings are resolved from a dynamic library upon program startup.

`c.bind_lib` tells the compiler where to look for the dynamic library. The extern declarations in the same module are then resolved at runtime:
```cy
#c.bind_lib('mylib.dylib')
```

A module can be configured to use static binding when building an executable or a runtime binding for faster iterations. The same `#extern` declarations are reused in both cases:
```cy
#if meta.is_vm_target():
    #c.bind_lib('mylib.dylib')
```

### `open_lib` binding.
*TODO*

If the path argument to `open_lib` is just a filename, the search steps for the library is specific to the operating system. Provide an absolute (eg. '/foo/mylib.so') or relative (eg. './mylib.so') path to load from a direct location instead. When the path argument is `none`, it loads the currently running executable as a library.

### `extern` functions.
An `extern` function is configured with a C call convention and registered for static or runtime binding:
```cy
#[extern]
fn SDL_CreateWindow(title Ptr[c_char], w c_int, h c_int, flags WindowFlags) -> Ptr[Window]
```

An extern symbol name can be declared if it differs from the API name:
```cy
#[extern='SDL_CreateWindow']
fn CreateWindow(title Ptr[c_char], w c_int, h c_int, flags WindowFlags) -> Ptr[Window]
```

The example above would be bound to this C function:
```c
SDL_Window* SDL_CreateWindow(const char *title, int w, int h, SDL_WindowFlags flags);
```

### `extern` globals.
An `extern` global is registered for static or runtime binding: *TBD*
```cy
#[extern] global count Ptr[int]
```
Note that an `extern` global requires a pointer type.

## cbindgen.cy
[cbindgen.cy](https://github.com/fubark/cyber/blob/master/src/tools/cbindgen.cy) is a script that automatically generates bindings given a C header file. Some example bindings that were generated include: [Raylib](https://github.com/fubark/ray-cyber), [SDL3](https://github.com/fubark/cyber/blob/master/src/x/sdl), [Vulkan](https://github.com/fubark/cyber/blob/master/src/x/vk), and [LLVM](https://github.com/fubark/cyber/blob/master/src/tools/llvm.cy).

# libcyber.

<table><tr>
<td valign="top">

* [Create VM.](#create-vm)
* [Override `print`.](#override-print)
* [Eval script.](#eval-script)
  * [Eval return.](#eval-return)
* [Module loader.](#module-loader)
* [Function binding.](#function-binding)
  * [VM functions.](#vm-functions)
* [Global binding.](#global-binding)
* [Type binding.](#type-binding)
</td><td valign="top">

</td>
</tr></table>

[^top](#table-of-contents)

`libcyber` allows embedding the Cyber compiler and VM into an application. Cyber's builtin types, functions, and the CLI app were built using `libcyber`.

The API is defined in the [C header file](https://github.com/fubark/cyber/blob/master/src/include/cyber.h).
The examples shown below can be found in the repository under [libcyber](https://github.com/fubark/cyber/blob/master/examples/libcyber). The examples are in C, but it can be easily translated to C++ or any C-ABI compatible language.

Types and constants from the C-API begin with `CL` and functions begin with `cl`.

## Create VM.
A VM instance is required to compile and interpret Cyber code. To create a new VM instance, call `cl_vm_init`:
```c
#include "cyber.h"

int main() {
    CLVM* vm = cl_vm_init();
    // ...
    cl_vm_deinit(vm);
    return 0;
}
```

## Override `print`.
The builtin `print` function does nothing by default, so it needs to be overrided to print to stdout for example:
```c
void printer(CLThread* t, CLBytes str) {
    printf("Invoked printer: %.*s\n", (int)str.len, str.buf);
}

int main() {
    // ...
    cl_vm_set_printer(vm, printer);
    // ...
}
```
Note that `prints` invokes printer once. But `print` invokes the printer twice, once for the value's string and another for the new line character.

Similarly, `eprint`, and `log` can also be overridden:
```c
void eprinter(CLThread* vm, CLBytes str) {
    fprintf(stderr, "Invoked eprinter: %.*s\n", (int)str.len, str.buf);
}

void logger(CLThread* vm, CLBytes str) {
    fprintf(stderr, "Invoked log: %.*s\n", (int)str.len, str.buf);
}

int main() {
    // ...
    cl_vm_set_eprinter(vm, eprinter);
    cl_vm_set_logger(vm, logger);
    // ...
}
```

## Eval script.
`cl_vm_eval` compiles and evaluates a script.
```c
CLBytes src = CL_BYTES(
    "a := 1\n"
    "print(a + 2)\n"
);

CLEvalResult res;
CLResultCode code = cl_vm_eval(vm, src, &res);
if (code == CL_SUCCESS) {
    printf("Success!\n");
} else {
    CLBytes summary = cl_vm_error_summary(vm);
    printf("%.*s\n", (int)summary.len, summary.buf);
    cl_vm_freeb(vm, summary);
}
```
`cl_vm_eval` returns a result code that indicates whether it was successful.

### Eval return.
To return a value back to the host, a main function with a return type is required:
```c
CLBytes src = CL_BYTES(
    "fn main() -> int:"
    "  a := 1\n"
    "  return a + 2\n"
);

CLEvalResult res;
cl_vm_eval(vm, src, &res);
if (code == CL_SUCCESS) {
    printf("returned %lld\n", *(int64_t*)res.res);
}
```

## Module loader.
A module loader is set with `cl_vm_set_loader`.
It describes how a module is loaded when triggered by a `use` import statement:
```c
bool loader(CLVM* vm, CLSym* mod, CLBytes uri, CLLoaderResult* res) {
    if (strncmp("my_mod", uri.ptr, uri.len) == 0) {
        const char* src = (
            "#[bind] fn add(a, b float) -> float\n"
            "#[bind] global my_global float\n"
            "\n"
            "type MyNode:\n"
            "  data float\n"
            "\n"
            "#[bind] fn MyNode :: @init() -> MyNode\n"
            "#[bind] fn (&MyNode) @deinit()\n"
            "#[bind] fn (&MyNode) compute() -> float\n"
        );

        cl_mod_add_func(mod, CL_BYTES("add"), CL_BIND_FUNC(add));
        cl_mod_add_func(mod, CL_BYTES("MyNode.@init"), CL_BIND_FUNC(mynode_init));
        cl_mod_add_func(mod, CL_BYTES("MyNode.@deinit"), CL_BIND_FUNC(mynode_deinit));
        cl_mod_add_func(mod, CL_BYTES("MyNode.compute"), CL_BIND_FUNC(mynode_compute));
        cl_mod_add_global(mod, CL_BYTES("my_global"), CL_BIND_GLOBAL(&myglobal));

        res->src = CL_BYTES(src);
        return true;
    } else {
        // Fallback to the default module loader to load builtin modules such as `core`.
        return cl_default_loader(vm, mod, uri, res);
    }
}

int main() {
    //...
    cl_vm_set_loader(vm, loader);
    //...
}
```
The above example loads `my_mod` by setting the appropriate function and global bindings, and returning its source code. Other modules get delegated to the default loader `cl_default_loader` which knows how to load the builtin modules such as `core`.

## Function binding.
A function binding describes how to load a `#[bind]` function.

### VM functions.
A VM function binding is created with `CL_BIND_FUNC()` and mapped to a module function name with `cl_mod_add_func`:
```c
// ...
cl_mod_add_func(mod, CL_BYTES("add"), CL_BIND_FUNC(add));
// ...
```

A VM function is a C function that the VM can call into. Cyber is a statically typed language so it requires the return and parameters to be defined by how much space they occupy on the stack:
```c
CLRet add(CLThread* t) {
    double* ret = cl_thread_ret(t, sizeof(double));
    double a = cl_thread_float(t);
    double b = cl_thread_float(t);
    *ret = a * b;
    return CL_RET_OK;
}
```

## Global binding.
A global binding describes how to load a `#[bind]` global:
```c
double myglobal = 234.0;

// ..
cl_mod_add_global(mod, CL_BYTES("my_global"), CL_BIND_GLOBAL(&myglobal));
// ..
```

## Type binding.
A type binding describes how to load a `#[bind]` type:
*TODO: example*

Types in Cyber have the same memory layout as a C type (assuming the member types are mapped correctly):

```cy
-- Cyber
type MyNode:
    data float
```

```c
// C
typedef struct MyNode {
    double data;
} MyNode;
```

# CLI.

* [Run program.](#run-program)
* [Help.](#help)
* [REPL.](#repl)
* [JIT compiler.](#jit-compiler)
* [C backend.](#c-backend)

[^top](#table-of-contents)

## Run program.
When given the main source file, `cyber` will compile and run a program in a VM: 
```bash
cyber main.cy
cyber path/to/main.cy
```

## Help.
To see more options and commands, print the help screen:
```bash
cyber help

# These are aliases to the help command.
cyber -h
cyber --help
```

## REPL.
The default behavior of `cyber` is to start a REPL:
```bash
cyber
> a := 123
> a * 2
`int` 246
```

Unlike conventional Cyber code, the REPL allows variable redeclarations:
```bash
> a := 'a is now a string'
> a
`str` 'a is now a string'
```

When the first input ends with `:`, the REPL will automatically indent the next line. To recede the indentation, provide an empty input. Once the indent returns to the beginning, the entire code block is submitted for evaluation:
```bash
> if true:
    | print('hello!')
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

## JIT compiler.
Cyber's just-in-time compiler is incomplete and unstable. To run your script with JIT enabled:
```bash
cyber -jit &lt;script&gt;
```

The goal of the JIT compiler is to be fast at compilation while still being significantly faster than the interpreter. The codegen involves stitching together pregenerated machine code that targets the same runtime stack slots used by the VM. This technique is also known as `copy-and-patch`.

## C backend.
The C backend generates a static binary from Cyber source code by transpiling to C code and relying on a local C compiler.
The user can specify the system's `cc` compiler or the builtin `tinyc` compiler that is bundled with the CLI.
*This is currently in progress.*

&nbsp;

&nbsp;