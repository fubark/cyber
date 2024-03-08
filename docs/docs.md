# Table of Contents.
- [Introduction.](#introduction)
- [Syntax.](#syntax)
- [Basic Types.](#basic-types)
- [Custom Types.](#custom-types)
- [Control Flow.](#control-flow)
- [Functions.](#functions)
- [Modules.](#modules)
- [FFI.](#ffi)
- [Error Handling.](#error-handling)
- [Concurrency.](#concurrency)
- [Type System.](#type-system)
- [Metaprogramming.](#metaprogramming)
- [Embedding.](#embedding)
- [Memory.](#memory)
- [Backends.](#backends)

<!--TOC-END-->

<!--This does not contain module docs. Use docs.md from releases instead or generate it by using docs/gen-docs.cy -->

# Introduction.

Cyber is a fast, efficient, and concurrent scripting language. The landing page is at [cyberscript.dev](https://cyberscript.dev) and contains performance metrics and release notes.

Cyber is easy to learn. These docs provide a reference manual for the language. You can read it in order or jump around using the navigation.

You may come across features that are marked `Incomplete` or `Planned`. This is because the docs are written as if all features have been completed already.

## Hello World.
```cy
import math

var worlds = ['World', '世界', 'दुनिया', 'mundo']
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
  * [Dynamic variables.](#dynamic-variables)
  * [Variable scopes.](#variable-scopes)
  * [Static variables.](#static-variables)
* [Reserved identifiers.](#reserved-identifiers)
  * [Keywords.](#keywords)
  * [Contextual keywords.](#contextual-keywords)
  * [Symbols.](#symbols)
  * [Literals.](#literals)
</td><td valign="top">

* [Operators.](#operators)
  * [Arithmetic operators.](#arithmetic-operators)
  * [Comparison operators.](#comparison-operators)
  * [Logic operators.](#logic-operators)
  * [Bitwise operators.](#bitwise-operators)
  * [Operator overloading.](#operator-overloading)
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

Any token inside a delimited syntax (such as parentheses or brackets) can be wrapped to the next line:
```cy
var sum = add(1, 2, 3, 4,
    100, 200, 300, 400)

var colors = ['red', 'blue', 'green',
    'purple', 'orange', 'yellow']
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
var items = [10, 20, 30]
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
Cyber supports dynamic and static typing. If you're used to a dynamic language such as Python, JavaScript, or Lua, use `my` to declare your variables and object fields. If you're used to a static language then use `var` instead. The documentation will use static typing by default.

### Local variables.
Local variables exist until the end of their scope.
They are declared and initialized using the `var` keyword:
```cy
var a = 123
```
When declared without a type specifier next to the variable, it infers the type from the right initializer.
To declare variables for a specific type, see [Typed variables](#typed-variables). 

Variables can be set afterwards using the `=` operator:
```cy
a = 234
```

### Dynamic variables.
Dynamically typed variables are easier to work with and there is no friction when using them. They are declared using the `my` keyword:
```cy
my a = 123
```
To understand more about dynamically and statically typed code, see [Type System](#type-system).

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
var .a = 123

func foo():
    print a     -- '123'
```
The `.` prefix is used to reference the current module's namespace.

Since static variables are initialized outside of a fiber's execution flow, they can not reference any local variables:
```cy
-- Static declaration.
var .b = a   -- Compile error, initializer can not reference a local variable.

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

print a        -- '444'
```

Circular references in initializers are not allowed.
When initialization encounters a reference that creates a circular dependency an error is reported.
```cy
var .a = b
var .b = a       -- CompileError. Referencing `a` creates a circular dependency.
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

## Reserved identifiers.

### Keywords.
There are `26` general keywords. This list categorizes them:

- [Control Flow](#control-flow): `if` `else` `switch` `case` `while` `for` `break` `continue` `pass`
- [Operators](#operators): `or` `and` `not`
- [Variables](#variables): `var` `my`
- [Functions](#functions): `func` `return`
- [Coroutines](#fibers): `coinit` `coyield` `coresume`
- [Types](#custom-types): `type`  `as`
- [Metaprogramming](#metaprogramming): `template`
- [Error Handling](#error-handling): `try` `catch` `throw`
- [Modules](#modules): `import`

### Contextual keywords.
These keywords only have meaning in a certain context.
- [Methods](#methods): `self` `Self`
- [Types](#custom-types): [`object`](#objects) [`struct`](#structs) [`enum`](#enums) 
- [Catching Errors](#caught-variable): `caught`
- [Function Throws](#throws-specifier): `throws`

### Literals.
- [Booleans](#booleans): `true` `false`
- [Error Values](#error-value): `error`
- [None](#none): `none`

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

my a = 'abc'
a == 'abc'  -- Evaluates to `true`

a = []
my b = a
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
[
    name: 'John Doe',
    'age': 25,
    -- This is a comment
    cities: [
        'New York',
        'San Francisco',
        'Tokyo',
    ],
]
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
  * [String operations.](#string-operations)
  * [String interpolation.](#string-interpolation)
  * [String formatting.](#string-formatting)
  * [Line-join literal.](#line-join-literal)
  * [Mutable strings.](#mutable-strings) 
</td>
<td valign="top">

* [Arrays.](#arrays)
* [Lists.](#lists)
* [Tuples.](#tuples)
* [Maps.](#maps)
  * [Create map.](#create-map)
  * [Empty map.](#empty-map)
  * [Map indexing.](#map-indexing)
  * [Map operations.](#map-operations)
  * [Map block.](#map-block)
* [Symbols.](#symbols-1)
</td>
</tr></table>

[^top](#table-of-contents)

In Cyber, there are primitive types and object types. By default, primitives are copied around by value and don't need additional heap memory or reference counts.

Primitives include [Booleans](#booleans), [Floats](#floats), [Integers](#integers), [Enums](#enums), [Symbols](#symbols-1), and [Error Values](#error-value).

Object types include [Lists](#lists), [Tuples](#tuples), [Maps](#maps), [Strings](#strings), [Arrays](#arrays), [Objects](#objects), [Lambdas](#lambdas), [Fibers](#fibers), [Choices](#choices), [Optionals](#optionals), [Pointers](#pointers), and several internal object types.

## Booleans.
Booleans can be `true` or `false`. See [`type bool`](#type-bool-struct).
```cy
var a = true
if true:
    print 'a is true'
```

## Numbers.

### Integers.
`int` is the default integer type. It has 48-bits and can represent integers in the range -(2<sup>47</sup>) to 2<sup>47</sup>-1. See [`type int`](#type-int-struct).

When a numeric literal is used and the type can not be inferred, it will default to the `int` type:
```cy
var a = 123
```

Integer notations always produce a `int` value:
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
`float` is the default floating point type. It has a (IEEE 754) 64-bit floating point format. See [`type float`](#type-float-struct).

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
The `String` type represents a sequence of validated UTF-8 codepoints, also known as `runes`. Each rune is stored internally as 1-4 bytes and can be represented as an `int`. See [`type String`](#type-string).

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
The following escape sequences are supported in string literals:

| Escape Sequence | Code | Description |
| --- | --- | --- |
| \a | 0x07 | Terminal bell. |
| \b | 0x08 | Backspace. |
| \e | 0x1b | Escape character. |
| \n | 0x0a | Line feed character. |
| \r | 0x0d | Carriage return character. |
| \t | 0x09 | Horizontal tab character. |

### String operations.
See [`type String`](#type-string) for all available methods.

Concatenate two strings together with the `+` operator or the method `concat`. 
```cy
var res = 'abc' + 'xyz'
res = res.concat('end')
```

Using the index operator will return the UTF-8 rune at the given index as a slice. This is equivalent to calling the method `sliceAt()`.
```cy
var str = 'abcd'
print str[1]     -- "b"
```

Using the slice operator will return a view of the string at the given start and end (exclusive) indexes. The start index defaults to 0 and the end index defaults to the string's length.
```cy
var str = 'abcxyz'
var sub = str[0..3]
print sub        -- "abc"
print str[..5]   -- "abcxy"
print str[1..]   -- "bcxyz"

-- One way to use slices is to continue a string operation.
str = 'abcabcabc'
var i = str.findRune(`c`)
print(i)                            -- "2"
i += 1
print(i + str[i..].findRune(`c`))   -- "5"
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
var paragraph = [
    \'the line-join literal
    \'hello\nworld
    \"hello $(name)
    \'last line
    \'
]
```

### Mutable strings.
To mutate an existing string, use [type MutString](#mutstring). *Planned Feature*

## Arrays.
An `Array` is an immutable sequence of bytes.
It can be a more performant way to represent strings but it won't automatically validate their encoding and indexing returns the n'th byte rather than a UTF-8 rune.
See [`type Array`](#type-array).

```cy
var a = Array('abcd')
a = a.insertByte(1, 255)
print a[0]     -- "97"
print a[1]     -- "255"
```

## Lists.
Lists are a builtin type that holds an ordered collection of elements. Lists grow or shrink as you insert or remove elements. See [`type List`](#type-list).
```cy
-- Construct a new list.
var list = [1, 2, 3]

-- The first element of the list starts at index 0.
print list[0]    -- Prints '1'
```

Lists can be sliced with the range `..` clause. The sliced list becomes a new list that you can modify without affecting the original list. The end index is non-inclusive.
```cy
var list = [ 1, 2, 3, 4, 5 ]
list[0..0]    -- []          
list[0..3]    -- [ 1, 2, 3 ] 
list[3..]     -- [ 4, 5 ]    
list[..3]     -- [ 1, 2, 3 ] 
```

The `+..` invokes the slice operator with an end position that is an increment from the start: *Planned Feature*

```cy
var list = [ 1, 2, 3, 4, 5 ]
list[2+..2]   -- [ 3, 4 ]
```

List operations.
```cy
var list = [234]

-- Append a value.
list.append 123

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

## Tuples.
> _Incomplete: Tuples can only be created from #host funcs at the moment._

## Maps.
Maps are a builtin type that store key value pairs in dictionaries. See [`type Map`](#type-map).

### Create map.
Create a map using key value pairs inside a collection literal:
```cy
var map = [ a: 123, b: () => 5 ]
```

Maps entries can be listed in multiple lines:
```cy
var map = [
    foo: 1,
    bar: 2,
]
```

### Empty map.
The empty map is initialized using `[:]`:
```cy
var empty = [:]
```

### Map indexing.
Get a value from the map using the index operator:
```cy
print map['a']
```

Maps can be accessed with the `.` dot operator as well:
```cy
print map.a
```

### Map operations.
```cy
var map = [:]

-- Set a key value pair.
map[123] = 234

-- Get the size of the map.
print map.size()

-- Remove an entry by key.
map.remove 123

-- Iterating a map.
for map -> [val, key]:
    print "$(key) -> $(val)"
```

### Map block.
Entries can also follow a collection literal block.
This gives structure to the entries and has
the added benefit of allowing multi-line lambdas.
*Planned Feature*
```cy
var colors = []:
    red: 0xFF0000
    green: 0x00FF00
    blue: 0x0000FF
    dump func (c):
        print c.red
        print c.green
        print c.blue

    -- Nested map.
    darker []: 
        red: 0xAA0000
        green: 0x00AA00
        blue: 0x0000AA
```

## Symbols.
Symbol literals begin with `.`, followed by an identifier. They have their own global unique id.
```cy
var currency = .usd
print(currency == .usd)   -- 'true'
print int(currency)       -- '123' or some arbitrary id.
```

# Custom Types.
<table><tr>
<td valign="top">

* [Objects.](#objects)
  * [Fields.](#fields)
  * [Instantiate.](#instantiate)
  * [Default field values.](#default-field-values)
  * [Circular references.](#circular-references)
  * [Unnamed object.](#unnamed-object)
  * [Methods.](#methods)
  * [`self` variable.](#self-variable)
  * [Type functions.](#type-functions)
  * [Type variables.](#type-variables)
* [Structs.](#structs)
  * [Declare struct.](#declare-struct)
  * [Copy structs.](#copy-structs)
* [Enums.](#enums)
* [Choices.](#choices)
  * [Initialize choice.](#initialize-choice)
  * [Choice `switch`.](#choice-switch)
  * [Access choice.](#access-choice)
</td><td valign="top">

* [Optionals.](#optionals)
  * [Wrap value.](#wrap-value)
  * [Wrap `none`.](#wrap-none)
  * [Unwrap or panic.](#unwrap-or-panic)
  * [Unwrap or default.](#unwrap-or-default)
  * [Optional chaining.](#optional-chaining)
  * [`if` unwrap.](#if-unwrap)
  * [`while` unwrap.](#while-unwrap)
* [Type aliases.](#type-aliases)
* [Type copies.](#type-copies)
* [Traits.](#traits)
* [Union types.](#union-types)
* [Generic types.](#generic-types)
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

### Fields.
Fields must be declared at the top of the `type` block with their names and type specifiers:
```cy
type Node:
    value int
    next  ?Node
```

### Instantiate.
New object instances are created using a record literal with a leading type name:
```cy
var node = [Node value: 123, next: none]
print node.value       -- Prints "123"
```

A record literal can also initialize to the inferred object type:
```cy
var node Node = [value: 234, next: none]
print node.value       -- Prints "234"
```

### Default field values.
When a field is omitted in the record literal, it gets initialized to its [zero value](#zero-values):
```cy
var node Node = [value: 234]
print node.next       -- Prints "Option.none"

type Student:
    name String
    age  int
    gpa  float

var s = [Student:]
print s.name       -- Prints ""
print s.age        -- Prints "0"
print s.gpa        -- Prints "0.0"
```

### Circular references.
Circular type references are allowed if the object can be initialized:
> _Planned Feature: Optional types are not currently supported._
```cy
type Node:
    val  any
    next ?Node

var n = [Node:]    -- Initializes.
```
In this example, `next` has an optional `?Node` type so it can be initialized to `none` when creating a new `Node` object.

The following example will fail because this version of `Node` can not be initialized:
```cy
type Node:
    val  any
    next Node

var n = [Node:]    -- CompileError. Can not zero initialize `next`
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

var node = [Node
    value: [a: 123, b: 100.0],
    next: none,
]
```

### Methods.
Methods allow invoking a function on an object instance using the `.` operator:
```cy
type Node:
    value int
    next  ?Node

    func inc(n):
        value += n

    func incAndPrint():
        self.inc(321)
        print value

var n = [Node value: 123, next: none]
n.incAndPrint()         -- Prints "444"
```

Methods can be declared outside of the type declaration. When using the flat declaration style, `self` must be the first parameter to distinguish it from a type function:
```cy
func Node.getNext(self):
    return self.next
```

### `self` variable.
Type members can be implicitly referenced inside the method. *Incomplete: Only the type's fields can be referenced this way.*

To reference members explicitly inside a method, use the builtin `self`:
```cy
type Node:
    value int
    next  ?Node

    func double():
        return self.value * 2
```

### Type functions.
Type functions are declared outside of the `type` block with an explicit namespace path:
```cy
type Node:
    value int
    next  ?Node

-- Declare namespace function inside `Node`.
func Node.new():
    return [Node value: 123, next: none]

var n = Node.new()
```

### Type variables.
Similarily, type variables are declared outside of the `type` block:
```cy
-- Declare inside the `Node` namespace.
var Node.DefaultValue = 100

print Node.DefaultValue    -- Prints "100"
```

## Structs.
Struct types can contain field and method members just like object types, but their instances are copied by value rather than by reference. In that sense, they behave like primitive data types.

Unlike objects, structs do not have a reference count. They can be safely referenced using borrow semantics. Unsafe pointers can also reference structs.

### Declare struct.
Struct types are created using the `type struct` declaration:
```cy
type Vec2 struct:
    x float
    y float

var v = [Vec2 x: 30, y: 40]
```

### Copy structs.
Since structs are copied by value, assigning a struct to another variable creates a new struct:
```cy
var v = [Vec2 x: 30, y: 40]
var w = v
v.x = 100
print w.x    -- Prints '30'
print v.x    -- Prints '100'
```

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
If the payload is an object type, the choice can be initialized with a simplified record literal:
```cy
var s = [Shape.rectangle width: 10, height: 20]
```

The general way to initialize a choice is to pass the payload as an argument:
```cy
var rect = [Rectangle width: 10, height: 20]
var s = [Shape rectangle: rect]

s = [Shape line: 20]
```

A choice without a payload is initialized with an empty record literal:
```cy
var s = [Shape.point:]
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
var s = [Shape line: 20]
print s.!line     -- Prints '20'
```

## Optionals.
The generic `Option` type is a choice type that either holds a `none` value or contains `some` value. The option template is defined as:
```cy
template(T type)
type Option enum:
    case none
    case some #T
```
A type prefixed with `?` is the idiomatic way to create an option type. The following String optional types are equivalent:
```cy
Option(String)
?String
```

### Wrap value.
Use `?` as a type prefix to turn it into an `Option` type. A value is automatically wrapped into the inferred optional's `some` case:
```cy
var a ?String = 'abc'
print a     -- Prints 'Option(abc)'
```

### Wrap `none`.
`none` is automatically initialized to the inferred optional's `none` case:
```cy
var a ?String = none
print a     -- Prints 'Option.none'
```

### Unwrap or panic.
The `.?` access operator is used to unwrap an optional. If the expression evaluates to the `none` case, the runtime panics:
```cy
var opt ?String = 'abc'
var v = opt.?
print v     -- Prints 'abc'
```

### Unwrap or default.
The `?else` control flow operator either returns the unwrapped value or a default value when the optional is `none`: *Planned Feature*
```cy
var opt ?String = none
var v = opt ?else 'empty'
print v     -- Prints 'empty'
```

`else` can also start a new statement block: *Planned Feature*
```cy
var v = opt else:
    break 'empty'

var v = opt else:
    throw error.Missing
```

### Optional chaining.
Given the last member's type `T` in a chain of `?.` access operators, the chain's execution will either return `Option(T).none` on the first encounter of `none` or returns the last member as an `Option(T).some`: *Planned Feature*
```cy
print root?.a?.b?.c?.last
```

### `if` unwrap.
The `if` statement can be amended to unwrap an optional value using the capture `->` operator: *Planned Feature*
```cy
var opt ?String = 'abc'
if opt -> v:
    print v     -- Prints 'abc'
```

### `while` unwrap. 
The `while` statement can be amended to unwrap an optional value using the capture `->` operator.
The loop exits when `none` is encountered: *Planned Feature*
```cy
var iter = dir.walk()
while iter.next() -> entry:
    print entry.name
```

## Type aliases.
A type alias refers to a different target type.
Once declared, the alias and the target type can be used interchangeably.

A type alias is declared using the assignment `=` operator after a `type` name declaration:
```cy
type Vec2:
    x float
    y float

type Pos2 = Vec2

var pos = [Pos2 x: 3, y: 4]
```

## Type copies.
A type copy creates a new type using the same type and memory layout of a target type.

It's declared with `type` name declaration followed by the target type specifier:
```cy
type Vec2:
    x float
    y float

type Pos2 Vec2

var pos = [Pos2 x: 3, y: 4]
```

Functions can be declared under the new type's namespace:
```
import math

type Pos2 Vec2:
    func blockDist(o Pos2):
        var dx = math.abs(o.x - x)
        var dy = math.abs(o.y - y)
        return dx + dy

var pos = [Pos2 x: 3, y: 4]
var dst = [Pos2 x: 4, y: 5]
print pos.blockDist(dst)     --> 2
```
Note that functions declared from the target type do not carry over to the new type.

Unlike a type alias, the new type and the target type can not be used interchangeably since they are different types. However, instances of the new type can be casted to the target type, and vice versa: *Planned Feature*
```cy
type Pos2 Vec2

var a = [Pos2 x: 3, y: 4]

var b Vec2 = a as Vec2
```

## Traits.
> _Planned Feature_

## Union types.
> _Planned Feature_

## Generic types.
Templates are used to specialize type declarations. Since template parameters only exist at compile-time, the `#` prefix is used to reference them in the template body:
```cy
template(T type)
type MyContainer:
    id    int
    value #T

    func get() #T:
        return self.value
```

### Expand type template.
When the template is invoked with compile-time argument(s), a specialized version of the type is generated.

In this example, `String` can be used as an argument since it satisfies the `type` parameter constraint:
```cy
var a MyContainer(String) = [id: 123, value: 'abc']
print a.get()      -- Prints 'abc'
```
Note that invoking the template again with the same argument(s) returns the same generated type. In other words, the generated type is always memoized from the input parameters.

# Control Flow.
<table><tr>
<td valign="top">

* [Branching.](#branching)
  * [`if` statement.](#if-statement)
  * [Conditional expression.](#conditional-expression)
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

### Conditional expression.
A conditional branch expression evaluates a condition and returns either the true value or false value:
```cy
var a = 10
var str = a == 10 ? 'red' else 'blue'
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
var list = [1, 2, 3, 4, 5]

for list -> n:
    print n
```

Maps can be iterated. `next()` returns a key and value tuple:
```cy
var map = [ a: 123, b: 234 ]

for map -> entry:
    print entry[0]
    print entry[1]
```

Use the destructure syntax to extract the key and value into two separate variables:
```cy
for map -> [ key, val ]:
    print "key $(key) -> value $(val)"
```

### `for` each with index.
A counting index can be declared after the each variable. The count starts at 0 for the first value:
```cy
var list = [1, 2, 3, 4, 5]

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
case 0..100: print 'at or between 0 and 99'
case 100   : print 'val is 100'
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
* [Named parameters.](#named-parameters)
* [Optional parameters.](#optional-parameters)
* [Variadic parameters.](#variadic-parameters)
* [Function calls.](#function-calls)
  * [Shorthand syntax.](#shorthand-syntax)
  * [Call block syntax.](#call-block-syntax)
* [Generic functions.](#generic-functions)

[^top](#table-of-contents)

In Cyber, there are first-class functions (or function values) and static functions.

## Static functions.
Static functions are not initially values themselves. They allow function calls to be optimal since they don't need to resolve a dynamic value.

Static functions are declared with the `func` keyword and must have a name.
```cy
import math

func dist(x0 float, y0 float, x1 float, y1 float) float:
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
    return dist(0.0, 0.0, size, size)
    
print squareDist(dist, 30.0)
```

Functions can only return one value. However, the value can be destructured: *Planned Feature*
```cy
import {cos, sin} 'math'

func compute(rad):
    return [ cos(rad), sin(rad) ]

var [ x, y ] = compute(pi)
```

## Function overloading.
Functions can be overloaded by the number of parameters in its signature. [Typed functions](#functions-1) are further overloaded by their type signatures. 
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
Passing a lambda block as a call argument is only possible in a call block. *Planned Feature* See [Function calls](#function-calls).

## Closures.
Lambdas can capture local variables from parent blocks. This example shows the lambda `f` capturing `a` from the main scope: *Incomplete, only variables one parent block away can be captured.*
```cy
var a = 1
var f = func():
    return a + 2
print f()         -- "3"
```

The following lambda expression captures `a` from the function `add`:
```cy
func add():
    var a = 123
    return b => a + b
var addTo = add()
print addTo(10)   -- "133"
```

Like static variables, static functions can not reference local variables outside of their scope:
```cy
var a = 1
func foo():
    print a       -- CompileError: Undeclared variable `a`.
```

## Named parameters.
> _Planned Feature_

## Optional parameters.
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
var d = dist(x0: 10, x1: 20, y0: 30, y1: 40)
```

### Shorthand syntax.
The shorthand method for calling functions omits parentheses and commas. This only works for functions that accept parameters:
> _Incomplete: Only the most trivial cases work with the shorthand method. The case with operators being separated by spaces might not end up being implemented._
```cy
var d = dist 100 100 200 200  -- Calls the function `dist`.

func random():       -- Function with no parameters.
    return 4

var r = random       -- Returns the function itself as a value.
                     -- Does not call the function `random`.
r = random()         -- Calls the function `random`.
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

## Generic functions.
> _Planned Feature_

# Modules.
<table><tr>
<td valign="top">

* [Importing.](#importing)
* [Exporting.](#exporting)
* [Module URI.](#module-uri)
* [Visibility.](#visibility)
* [Builtin modules.](#builtin-modules)
* [builtins.](#builtins)
  * [`type bool`](#type-bool-struct)
  * [`type error`](#type-error-struct)
  * [`type int`](#type-int-struct)
  * [`type float`](#type-float-struct)
  * [`type List`](#type-list)
  * [`type ListIterator`](#type-listiterator)
  * [`type tuple`](#type-tuple)
  * [`type Map`](#type-map)
  * [`type MapIterator`](#type-mapiterator)
  * [`type String`](#type-string)
  * [`type Array`](#type-array)
  * [`type ArrayIterator`](#type-arrayiterator)
  * [`type pointer`](#type-pointer)
  * [`type Fiber`](#type-fiber)
  * [`type metatype`](#type-metatype)

</td><td valign="top">

* [math.](#math)
* [Std modules.](#std-modules)
* [os.](#os)
  * [`type File`](#type-file)
  * [`type Dir`](#type-dir)
  * [`type DirIterator`](#type-diriterator)
  * [`type FFI`](#type-ffi)
  * [`type CArray`](#type-carray)
  * [`type CDimArray`](#type-cdimarray)
  * [`type DirEntry`](#type-direntry)
  * [`type DirWalkEntry`](#type-dirwalkentry)
  * [`type ArgOption`](#type-argoption)
  * [`map DirEntry`](#map-direntry)
  * [`map DirWalkEntry`](#map-dirwalkentry)
  * [`map ArgOption`](#map-argoption)
* [test.](#test)
</td>
</tr></table>

[^top](#table-of-contents)

Modules have their own namespace and contain accessible static symbols. By default, importing another Cyber script returns a module with its declared symbols.

## Importing.
Import declarations create a local alias to the module referenced by the import specifier. The Cyber CLI comes with some builtin modules like `math` and `test`. If the specifier does not refer to a builtin module, it looks for a Cyber script file relative to the current script's directory. An embedder can integrate their own module loader and resolver.
```cy
import test
test.eq(123, 123)

-- Imports are static declarations so they can be anywhere in the script.
import math
print math.cos(0)
```

When the imported alias needs to be renamed, the import specifier comes after the alias name and must be a string literal.
```cy
import m 'math'
print m.random()

-- Loading a Cyber module from the local directory.
import foo 'bar.cy'
print foo.myFunc()
print foo.myVar
```

A Cyber script that is imported doesn't evaluate its main block. Only static declarations are effectively loaded. If there is code in the main block, it will skip evaluation. In the following, only the `print` statement in the `main.cy` is evaluated.
```cy
-- main.cy
import a 'foo.cy'
print a.foo

-- foo.cy
import 'bar.cy'
var .foo = 123
print foo         -- Statement is ignored.

-- bar.cy
var .bar = 321
print bar         -- Statement is ignored.
```
You can have circular imports in Cyber. In the following example, `main.cy` and `foo.cy` import each other without any problems.
```cy
-- main.cy
import foo 'foo.cy'

func printB():
    foo.printC()

foo.printA()

-- foo.cy
import main 'main.cy'

func printA():
    main.printB()

func printC():
    print 'done'
```
Static variable declarations from imports can have circular references. Read more about this in [Static Variables](#static-variables).

Modules can also be destructured using the following syntax:
> _Planned Feature_
```cy
import { cos, pi } 'math'
print cos(pi)
```

## Exporting.
All static declarations are exported when the script's module is loaded.
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

import os
print os.dirName(#modUri)  -- Prints '/some/path'
```

## Visibility.
The annotation `@hide` provides a hint to editors that a static symbol should not appear in the auto-complete. Despite this, the symbol is still reachable.

## Builtin modules.
Builtin modules are the bare minimum that comes with Cyber. The [embeddable library](#embedding) contains these modules and nothing more. They include:
- [builtins](#builtins): Cyber related functions and commonly used utilities.
- [math](#math): Math constants and functions.

## builtins.
The `builtins` module contains functions related to Cyber and common utilities. It is automatically imported into each script's namespace. 

Sample usage:
```cy
-- `print` and `typeof` are available without imports.
print 'hello'
print typeof('my str').id()
```

<!-- builtins.start -->
<!-- builtins.end -->

## math.
The math module contains commonly used math constants and functions.

Sample usage:
```cy
import math

var r = 10.0
print(math.pi * r^2)
```

<!-- math.start -->
<!-- math.end -->

## Std modules.
Std modules come with Cyber's CLI. They include:
- [os](#os): System level functions.
- [test](#test): Utilities for testing.

## os.
Cyber's os module contains system level functions. It's still undecided as to how much should be included here so it's incomplete. You can still access os and libc functions yourself using Cyber's FFI or embedding API.

Sample usage:
```cy
import os

var map = os.getEnvAll()
for map -> [k, v]:
    print "$(k) -> $(v)"
```

<!-- os.start -->
<!-- os.end -->
### `map DirEntry`
| key | summary |
| -- | -- |
| `'name' -> Array` | The name of the file or directory. |
| `'type' -> #file | #dir | #unknown` | The type of the entry. |

### `map DirWalkEntry`
| key | summary |
| -- | -- |
| `'name' -> Array` | The name of the file or directory. |
| `'path' -> Array` | The path of the file or directory relative to the walker's root directory. |
| `'type' -> #file | #dir | #unknown` | The type of the entry. |

### `map ArgOption`
| key | summary |
| -- | -- |
| `'name' -> String` | The name of the option to match excluding the hyphen prefix. eg. `-path` |
| `'type' -> metatype(String | float | boolean)` | Parse as given value type. |
| `'default' -> any` | Optional: Default value if option is missing. `none` is used if this is not provided. |

## test.
The `test` module contains utilities for testing.

Sample usage:
```cy
import t 'test'

var a = 123 + 321
t.eq(a, 444)
```

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
* [Pointers.](#pointers)
* [cbindgen.cy](#cbindgency)

[^top](#table-of-contents)

Cyber supports binding to an existing C ABI compatible library at runtime.
This allows you to call into dynamic libraries created in C or other languages.
Cyber uses `libtcc` to JIT compile the bindings so function calls are fast.
The example shown below can be found in [Examples](https://github.com/fubark/cyber/blob/master/examples/ffi.cy).

## FFI context.
An FFI context contains declarations that map C to Cyber. Afterwards, it allows you to bind to a dynamic library or create interoperable objects. To create a new `FFI` context:
```cy
import os

var ffi = os.newFFI()
```

## Declare functions.
Functions from a library are first declared using `cfunc` which accepts C types in the form of symbols. In a future update they will accept C syntax instead.
```cy
ffi.cfunc('add', [.int, .int], .int)
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
my lib = ffi.bindLib('./mylib.so')
lib.add(123, 321)
```
Note that `my` is used to allow `lib` to be used dynamically since the type is unknown at compile-time.

### Search path.
If the path argument to `bindLib` is just a filename, the search steps for the library is specific to the operating system. Provide an absolute (eg. '/foo/mylib.so') or relative (eg. './mylib.so') path to load from a direct location instead. When the path argument is `none`, it loads the currently running executable as a library allowing you to bind exported functions from the Cyber CLI or your own application/runtime.

### Configuration.
By default `bindLib` returns an anonymous object with the binded C-functions as methods. This is convenient for invoking functions using the method call syntax. If a config is passed into `bindLib` as the second argument, `genMap: true` makes `bindLib` return a map instead with the binded C-functions as Cyber functions.

### Finalizer.
The resulting object of `bindLib` holds a reference to an internal TCCState which owns the loaded JIT code.
Once the object is released by ARC, the TCCState is also released which removes the JIT code from memory.

## Mappings.
When using `cfunc` or `cbind` declarations, [symbols](#symbols-1) are used to represent default type mappings from Cyber to C and back:
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
| (1) .charPtr | pointer | char* |
| .voidPtr | pointer | void* |
| (2) type {S} | type {S} | struct |

1. Use `os.cstr()` and `pointer.fromCstr()` to convert between a Cyber string and a null terminated C string.
2. The mapping from a Cyber object type `S` and the C-struct can be declared with `cbind`.

## Bind to Cyber type.
`cbind` is used to bind a C struct to a Cyber object type. Once declared, the Cyber type can be used as a binding type in function declarations:
```cy
import os

type MyObject:
    a float
    b pointer
    c bool

ffi.cbind(MyObject, [.float, .voidPtr, .bool])
ffi.cfunc('foo', [MyObject], MyObject)
my lib = ffi.bindLib('./mylib.so')

var res = lib.foo([MyObject a: 123.0, b: os.cstr('foo'), c: true])
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
ffi.cfunc('foo', [MyObject], .voidPtr)
my lib = ffi.bindLib('./mylib.so')

var ptr = lib.foo([MyObject a: 123, b: os.cstr('foo'), c: true])
var res = lib.ptrToMyObject(ptr)
```

## Pointers.
A `pointer` is used to read or write to an exact memory address. This is typically used for FFI to manually map Cyber types to C, and back. See [`type pointer`](#type-pointer).

A new pointer can be created with the builtin `pointer`.
```cy
var ptr = pointer(0xDEADBEEF)
print ptr.value()     --'3735928559'
```

## cbindgen.cy
[cbindgen.cy](https://github.com/fubark/cyber/blob/master/src/tools/cbindgen.cy) is a Cyber script that automatically generates bindings given a C header file. Some example bindings that were generated include: [Raylib](https://github.com/fubark/ray-cyber) and [LLVM](https://github.com/fubark/cyber/blob/master/src/tools/llvm.cy).

# Error Handling.
<table><tr>
<td valign="top">

* [Error trait.](#error-trait)
  * [`error` value.](#error-value)
  * [Enum error.](#enum-error)
* [Throwing errors.](#throwing-errors)
* [Catching errors.](#catching-errors)
  * [`try` block.](#try-block)
  * [`caught` variable.](#caught-variable)
  * [`catch` matching.](#catch-matching)
  * [`try` expression.](#try-expression)
  * [Value or error.](#value-or-error)
</td><td valign="top">

* [Semantic checks.](#semantic-checks)
  * [`throws` specifier.](#throws-specifier)
  * [Requiring `throws`.](#requiring-throws)
* [Stack trace.](#stack-trace)
* [Unexpected errors.](#unexpected-errors)
  * [Panics.](#panics)
</td>
</tr></table>

[^top](#table-of-contents)

Cyber provides a throw/catch mechanism to handle expected errors. For unexpected errors, panics can be used as a fail-fast mechanism to abort the currently running fiber.

## Error trait.
Only types that implement the `Error` trait can be thrown or attached to a panic.
Since the `Error` trait is empty, it's simple to turn any type into a throwable type.

### `error` value.
An `error` value contains a `symbol` and implements the `Error` trait. They can be created without a declaration using the error literal:
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

### Enum error.
By implementing the `Error` trait, an enum type can be throwable: *Planned Feature*
```cy
type MyError enum:
    with Error
    boom
    badArgument
    nameTooLong

var err = MyError.nameTooLong
```

## Throwing errors.
Use the `throw` keyword to throw errors. 
A thrown error continues to bubble up the call stack until it is caught by a `try` block or expression.
```cy
func fail():
    throw error.Oops      -- Throws an error with the symbol `#Oops`

func fail2():
    throw 123             -- Panic. Can only throw an error
                          -- that implement the `Error` trait.
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

Enum errors can be matched: *Planned Feature*
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
### `throws` specifier.
The `throws` specifier indicates that a function contains a throwing expression that was not caught with `try catch`.

When a function does not have a return specifier, it's implicitly given the `throws` specifier:
```cy
func foo():
    throw error.Failure

func bar() throws:
    throw error.Failure

-- `foo` and `bar` both have the same return specifier.
```

Return types for typed functions are declared after `throws` using a comma separator:
```cy
func result(cond bool) throws, int:
    if cond:
        return 123
    else:
        throw error.Failure
```

### Requiring `throws`.
A compile-time error is issued when a typed function without a `throws` specifier contains an uncaught throwing expression: *Planned Feature*
```cy
func foo(a int) int:
    if a == 10:
        throw error.Failure -- CompileError. `foo` requires the `throws`
    else:                   -- specifier or any throwing expression must
        return a * 2        -- be caught with `try catch`.
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

* [Fibers.](#fibers)
  * [Creating fibers.](#creating-fibers)
  * [Passing arguments.](#passing-arguments)
  * [Reset state.](#reset-state)
  * [Rebinding arguments.](#rebinding-arguments)
  * [Fiber block.](#fiber-block)
  * [Pause and resume.](#pause-and-resume)
  * [Fiber state.](#fiber-state)
* [Gas mileage.](#gas-mileage)
* [Async.](#async)
* [Multi-thread.](#multi-thread)

[^top](#table-of-contents)

Cyber supports fibers as a concurrency mechanism. There are plans to support preemptive concurrency with async/await as well as multithreading.

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

var fiber = coinit(foo)

print count          -- '0'
coresume fiber
print count          -- '1'
coresume fiber
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

var fiber = coinit(increment, 5)
coresume fiber
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
fiber.reset()
print(coresume task)    -- Prints "10"
```

### Rebinding arguments.
Arguments attached to the fiber can be rebinded with a different set of values. *Planned Feature*
This allows fiber reuse, instead of creating a new fiber:
```cy
var task = coinit(fib, 10)

-- Run task to completion.
var res = 0
while fiber.status() != .done:
    res = coresume fiber
print res

fiber.reset()
fiber.bindArgs(20)

-- Run task again with the new argument...
```

### Fiber block.
A fiber block is used to construct a fiber without an entry function. *Planned Feature* The counting example can be rewritten to:
```cy
var count = 0

var fiber = coinit:
    count += 1       -- `count is captured`
    coyield
    count += 1

print count          -- '0'
coresume fiber
print count          -- '1'
coresume fiber
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

var fiber = coinit(foo)
coresume fiber
```
`coresume` also returns the resulting value.
```cy
func foo():
    return 123

var fiber = coinit(foo)
print(coresume fiber)    -- '123'
```

`coyield` can return a value back to `coresume`. *Planned Feature*

### Fiber state.
Use `Fiber.status()` to get the current state of the fiber.
```cy
func foo():
    coyield
    print 'done'

var fiber = coinit(foo)
print fiber.status()   -- '.paused'
coresume fiber
print fiber.status()   -- '.paused'
coresume fiber
print fiber.status()   -- '.done'
```
The main execution context is a fiber as well. Once the main fiber has finished, the VM is done and control is returned to the host.

## Gas mileage.
> _Planned Feature_

## Async.
> _Planned Feature_

## Multi-thread.
> _Planned Feature_

# Type System.

<table><tr>
<td valign="top">

* [Dynamic typing.](#dynamic-typing)
  * [`my` declaration.](#my-declaration)
  * [`dynamic` vs `any`.](#dynamic-vs-any)
  * [Invoking dynamic values.](#invoking-dynamic-values)
  * [Dynamic return value.](#dynamic-return-value)
  * [Recent type inference.](#recent-type-inference)
</td><td valign="top">

* [Static typing.](#static-typing)
  * [`var` declaration.](#var-declaration)
  * [Typed variables.](#typed-variables)
  * [Null safety.](#null-safety)
  * [Zero values.](#zero-values)
  * [Functions.](#functions-1)
  * [`any` type.](#any-type)
  * [Invoking any values.](#invoking-any-values)
  * [Type casting.](#type-casting)
</td>
</tr></table>

[^top](#table-of-contents)

Cyber supports the use of both dynamically and statically typed code.

## Dynamic typing.
Dynamic typing can reduce the amount of friction when writing code, but it can also result in more runtime errors.

### `my` declaration.
Variables declared with `my` are assigned the `dynamic` type:
```cy
my a = 123
```

### `dynamic` vs `any`
`dynamic` values can be freely used and copied without any compile errors (if there is a chance it can succeed at runtime, see [Recent type inference](#recent-type-inference)):
```cy
my a = 123

func getFirstRune(s String):
    return s[0]

getFirstRune(a)       -- RuntimeError. Expected `String`.
```
Since `a` is dynamic, passing it to a typed function parameter is allowed at compile-time, but will fail when the function is invoked at runtime.

The `any` type on the otherhand is a **static type** and must be explicitly declared using `var`:
```cy
var a any = 123

func getFirstRune(s String):
    return s[0]

getFirstRune(a)       -- CompileError. Expected `String`.
```
This same setup will now fail at compile-time because `any` does not satisfy the destination's `String` type constraint.

The use of the `dynamic` type effectively defers type checking to runtime while `any` is a static type and must adhere to type constraints at compile-time.

A `dynamic` value can be used in any operation. It can be invoked as the callee, invoked as the receiver of a method call, or used with operators.

### Invoking `dynamic` values.
When a `dynamic` value is invoked, checks on whether the callee is a function is deferred to runtime. 
```cy
my op = 123
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
my a = 123
```

The recent type can change at compile-time from another assignment. 
If `a` is then assigned to a string literal, `a` from that point on has the recent type of `String` at compile-time:
```cy
my a = 123
foo(a)           -- Valid call expression.
a = 'hello'
foo(a)           -- CompileError. Expected `int` argument, got `String`.

func foo(n int):
    pass
```
Even though `a` is `dynamic` and is usually allowed to defer type checking to runtime, the compiler knows that doing so in this context would **always** result in a runtime error, so it provides a compile error instead. This provides a quicker feedback to fix the problem.

The recent type of `a` can also change in branches. However, after the branch block, `a` will have a recent type after merging the types assigned to `a` from the two branched code paths. Currently, the `any` type is used if the types from the two branches differ. At the end of the following `if` block, `a` has the recent type of `any` type after merging the `int` and `String` types: *Planned Feature*
```cy
my a = 123
if a > 20:
    a = 'hello'
    foo(a)       -- Valid call expression. `foo` can be called without type casting.

foo(a)           -- CompileError. Expected `String` argument, got `any`.

func foo(s String):
    pass
```

## Static typing.
Static typing can be incrementally applied which provides compile-time guarantees and prevents runtime errors.
Static typing also makes it easier to maintain and refactor your code.

There are [basic types](#basic-types) that are built into Cyber and `type` declarations to create [custom types](#custom-types).

### `var` declaration.
A `var` declaration automatically infers the type from the initializer:
```cy
-- Initialized as an `int` variable.
var a = 123
```

`var` declarations are strictly for static typing. If the assigned value's type is `dynamic`, the variable's type becomes `any`.
```cy
func getValue():
    return ['a', 'list']

-- Initialized as an `any` variable.
var a = getValue()
```

### Typed variables.
A typed local variable can be declared by attaching a type specifier after its name. The value assigned to the variable must satisfy the type constraint or a compile error is issued.
```cy
var a float = 123

var b int = 123.0    -- CompileError. Expected `int`, got `float`.
```

Any operation afterwards that violates the type constraint of the variable will result in a compile error.
```cy
a = 'hello'          -- CompileError. Expected `float`, got `String`.
```

Static variables are declared in a similar way:
```cy
var .global Map = [:]
```
Unlike local variables, static variable declarations do not infer the type from the right hand side. A specific type must be specified or it will default to the `any` type.

### Null safety.
The type checker does not allow using `none` with an operation. Instead, [Optionals](#optionals) must be unwrapped to access the payload value.

### Zero values.
The following shows the zero values of builtin or created types.

|Type|Zero value|
|---|---|
|`boolean`|`false`|
|`int`|`0`|
|`float`|`0.0`|
|`String`|`''`|
|`Array`|`Array('')`|
|`List`|`[]`|
|`Map`|`[:]`|
|`type S`|`[S:]`|
|`#host type S`|`S.$zero()`|
|`dynamic`|`Option.none`|
|`any`|`Option.none`|
|`?S`|`Option(S).none`|

### Functions.
Function parameter and return type specifiers follows a similiar syntax.
```cy
func mul(a float, b float) float:
    return a * b

print mul(3, 4)
print mul(3, '4')  -- CompileError. Function signature mismatch.
```

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

var opFunc = op as (func (int, int) int)
print opFunc(1, 2)     -- Prints "3".
```

### Type casting.
The `as` keyword can be used to cast a value to a specific type. Casting lets the compiler know what the expected type is and does not perform any conversions.

If the compiler knows the cast will always fail at runtime, a compile error is returned instead.
```cy
print('123' as int)       -- CompileError. Can not cast `String` to `int`.
```

If the cast fails at runtime, a panic is returned.
```cy
var erased any = 123
add(1, erased as int)     -- Success.
print(erased as String)   -- Panic. Can not cast `int` to `String`.

func add(a int, b int):
    return a + b
```

# Metaprogramming.

<table><tr>
<td valign="top">

* [Operator overloading.](#operator-overloading-1)
  * [Builtin operators.](#builtin-operators)
  * [Custom operators.](#custom-operators)
* [Magic functions.](#magic-functions)
  * [Call module.](#call-module)
  * [Getter/Setter.](#gettersetter)
  * [Missing method.](#missing-method)
</td><td valign="top">

* [Reflection.](#reflection)
* [Directives.](#directives)
* [Generics.](#generics)
* [Macros.](#macros)
* [Compile-time execution.](#compile-time-execution)
  * [Builtin functions.](#builtin-functions)
  * [Builtin constants.](#builtin-constants)
* [Runtime execution.](#runtime-execution)
</td>
</tr></table>

[^top](#table-of-contents)

## Operator overloading.
All operators are implemented as object methods.
> _Incomplete: Not all operators have transitioned to the method paradigm._ 

Normally this would impact performance, but Cyber generates specialized bytecode for builtin types like `int` and `float`. The VM performs inline caching at runtime to eliminate the overhead of evaluating on dynamic operands.

To overload an operator for an object type, declare `$prefix`, `$infix`, `$postfix` methods. See the available [builtin operators](#builtin-operators). Since operator names aren't allowed as standard identifiers, they are contained in a string literal.
```cy
type Vec2:
    x float
    y float

    func '$infix+'(o Vec2) Vec2:
        return [Vec2
            x: x + o.x,
            y: y + o.y,
        ]

    func '$prefix-'() Vec2:
        return [Vec2 x: -x, y: -y]

var a = [Vec2 x: 1, y: 2]
var b = a + [Vec2 x: 3, y: 4]
var c = -a
```

Some special operators have their own name. This example overloads the `index` operator and the `set index` operator:
```cy
type MyCollection:
    arr List

    func '$index'(idx):
        return arr[idx * 2]

    func '$setIndex'(idx, val):
        arr[idx * 2] = val 

var a = [MyCollection arr: [1, 2, 3, 4]]
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

func Vec2.'$call'(x float, y float) Vec2:
    return [Vec2 x: x, y: y]

var v = Vec2(1, 2)
```

### Getter/Setter.
> _Planned Feature_

### Missing method.
Declare a `$missing` method as a fallback when a method was not found in an instance.
> _Planned Feature_
```cy
type A:

    func '$missing'(args...):
        return args.len

var a = [A:]
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

## Directives.
Directives start with `#` and are used as modifiers.
> `#host`
>
>Modifier to bind a function, variable, or type to the host. See [Embedding](#embedding).

## Generics.
Generics enables parametric polymorphism for types and functions. Compile-time arguments are passed to templates to generate specialized code. This facilitates developing container types and algorithms that operate on different types.

See [Custom Types / Generic types](#generic-types) and [Functions / Generic functions](#generic-functions).

## Macros.
> _Planned Feature_

## Compile-time execution.
> _Planned Feature_

### Builtin functions.
> `func genLabel(name String)`
>
>Emits a label during codegen for debugging.

### Builtin constants.
> `var modUri String`
>
>Evaluates to the module's URI as a string. See [Module URI](#module-uri).

## Runtime execution.
> _Planned Feature_

# Embedding.

<table><tr>
<td valign="top">

* [Getting started.](#getting-started)
  * [Create VM.](#create-vm)
  * [Override `print`.](#override-print)
  * [Eval script.](#eval-script)
* [Module Loader.](#module-loader)
  * [Default module loader.](#default-module-loader)
  * [Function loader.](#function-loader)
  * [Variable loader.](#variable-loader)
  * [Type loader.](#type-loader)
</td><td valign="top">

* [Host functions.](#host-functions)
* [Host types.](#host-types)
  * [`getChildren`](#getchildren)
  * [`finalizer`](#finalizer-1)
</td>
</tr></table>

[^top](#table-of-contents)

The Embed API allows embedding the Cyber compiler and VM as a library into applications. Cyber's core types and the CLI app were built using the Embed API.

The API is defined in the [C header file](https://github.com/fubark/cyber/blob/master/src/include/cyber.h).
The examples shown below can be found in the repository under [c-embedded](https://github.com/fubark/cyber/blob/master/examples/c-embedded). C is used as the host language, but it can be easily translated to C++ or any C-ABI compatible language.

Types from the Embed API begin with `Cs`, constants begin with `CS`, and functions begin with `cs`.

## Getting started.

### Create VM.
Most operations are tied to a VM handle. To create a new VM instance, call `csCreate`:
```c
#include "cyber.h"

int main() {
    CsVM* vm = csCreate();
    // ...
    csDestroy(vm);
    return 0;
}
```

### Override `print`.
The builtin `print` function does nothing by default, so it needs to be overrided to print to stdout for example:
```c
void print(CsVM* vm, CsStr str) {
    printf("My print: %.*s\n", (int)str.len, str.buf);
}

int main() {
    // ...
    csSetPrinter(vm, print);
    // ...
}
```

### Eval script.
`csEval` compiles and evaluates a script:
```c
CsStr src = STR(
    "var a = 1\n"
    "print(a + 2)\n"
);

CsValue val;
int res = csEval(vm, src, &val);
if (res == CS_SUCCESS) {
    printf("Success!\n");
    csRelease(vm, val);
} else {
    const char* report = csNewLastErrorReport(vm);
    printf("%s\n", report);
    csFreeStrZ(report);
}
```
If a value is returned from the main block of the script, it's saved to the result value argument.
Memory is managed by ARC so a value that points to a heap object requires a `csRelease` when it's no longer needed.

`csEval` returns a result code that indicates whether it was successful.

## Module Loader.
A module loader describes how a module is loaded when an `import` statement is encountered during script execution.
Only one module loader can be active and is set using `csSetModuleLoader`:
```c
bool modLoader(CsVM* vm, CsStr spec, CsModuleLoaderResult* out) {
    if (strncmp("my_mod", spec.buf, spec.len) == 0) {
        out->src =
            "#host func add(a float, b float) float\n"
            "#host var .MyConstant float\n"
            "#host var .MyList     List\n"
            "\n"
            "#host\n"
            "type MyCollection:\n"
            "    #host func asList() any"
            "\n"
            "#host func MyCollection.new(a, b) MyCollection\n";
        out->funcLoader = funcLoader;
        out->varLoader = varLoader;
        out->typeLoader = typeLoader;
        return true;
    } else {
        // Fallback to the default module loader to load `builtins`.
        return csDefaultModuleLoader(vm, spec, out);
    }
}

int main() {
    //...
    csSetModuleLoader(vm, modLoader);
    //...
}
```
The above example checks whether "my_mod" was imported and returns it's source code. Additional loaders are returned to load the functions, variables, and types from the source code.

### Default module loader.
Since only one module loader can be set to the VM instance, a custom loader is required to handle the "builtins" import which contains all of the core types and functions in Cyber. This can simply be delegated to `csDefaultModuleLoader`.

### Function loader.
A function loader describes how to load a `#host` function when it's encountered by the compiler.
The loader can bind functions and type methods:
```c
struct { char* n; CsFuncFn fn; } funcs[] = {
    {"add", add},
    {"asList", myCollectionAsList},
    {"MyCollection.new", myCollectionNew},
};

bool funcLoader(CsVM* vm, CsFuncInfo info, CsFuncResult* out) {
    // Check that the name matches before setting the function pointer.
    if (strncmp(funcs[info.idx].n, info.name.buf, info.name.len) == 0) {
        out->ptr = funcs[info.idx].fn;
        return true;
    } else {
        return false;
    }
}
```
This example uses the `CsFuncInfo.idx` of a #host function to index into an array and return a [Host function](#host-functions) pointer. The name is also compared to ensure it's binding to the correct pointer.

This is an efficient way to map Cyber functions to host functions. A different implementation might use a hash table to map the name of the function to it's pointer.

### Variable loader.
A variable loader describes how to load a `#host` variable when it's encountered by the compiler:
```c
// C has limited static initializers (and objects require a vm instance) so initialize them in `main`.
typedef struct { char* n; CsValue v; } NameValue;
NameValue vars[2];

bool varLoader(CsVM* vm, CsVarInfo info, CsValue* out) {
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
    vars[0] = (NameValue){".MyConstant", csFloat(1.23)};
    CsValue myInt = csInteger(123);
    vars[1] = (NameValue){".MyList", csNewList(vm, &myInt, 1)};

    // ...
}
```
This example uses the same technique as the function loader, but it can be much simpler. It doesn't matter how the mapping is done as long as the variable loader returns a `CsValue`.

### Type loader.
A type loader describes how to load a `#host` type when it's encountered by the compiler:
```c
CsTypeId myCollectionId;

bool typeLoader(CsVM* vm, CsTypeInfo info, CsTypeResult* out) {
    if (strncmp("MyCollection", info.name.buf, info.name.len) == 0) {
        out->type = CS_TYPE_OBJECT;
        out->data.object.outTypeId = &myCollectionId;
        out->data.object.getChildren = myCollectionGetChildren;
        out->data.object.finalizer = myCollectionFinalizer;
        return true;
    } else {
        return false;
    }
}
```
When binding to the "MyCollection" type, it's typeId is saved to `outTypeId`. This id is then used to create new instances of this type. See [Host types](#host-types).

## Host functions.
A host function requires a specific function signature:
```c
CsValue add(CsVM* vm, const CsValue* args, uint8_t nargs) {
    double res = csAsFloat(args[0]) + csAsFloat(args[1]);
    return csFloat(res);
}
```
A host function should always return a `CsValue`. `csNone()` can be returned if the function does not intend to return any value.

## Host types.
A host type are types that are opaque to Cyber scripts but still behave like an object. They can have type functions and methods.

Only the host application can directly create new instances of them, so usually a function is binded to expose a constructor to the user script:
```c
// Binding a C struct with it's own children and finalizer.
// This struct retains 2 VM values and has 2 arbitrary data values unrelated to the VM.
typedef struct MyCollection {
    CsValue val1;
    CsValue val2;
    int a;
    double b;
} MyCollection;

// Implement the `new` function in MyCollection.
CsValue myCollectionNew(CsVM* vm, const CsValue* args, uint8_t nargs) {
    // Instantiate our object.
    CsValue new = csNewHostObject(vm, myCollectionId, sizeof(MyCollection));
    MyCollection* my = (MyCollection*)csAsHostObject(new);

    // Assign the constructor args passed in and retain them since the new object now references them.
    csRetain(vm, args[0]);
    my->val1 = args[0];
    csRetain(vm, args[1]);
    my->val2 = args[1];

    // Assign non VM values.
    my->a = 123;
    my->b = 9999.999;
    return new;
}
```
`csNewHostObject` takes the type id (returned from the [Type loader](#type-loader)) and size (in bytes) and returns a new heap object. Note that the size is allowed to vary. Different instances of the same type can occupy different amounts of memory.

### `getChildren`
Since `MyCollection` contains `CsValue` children, the [Type loader](#type-loader) requires a `getChildren` callback so that memory management can reach them:
```c
CsValueSlice myCollectionGetChildren(CsVM* vm, void* obj) {
    MyCollection* my = (MyCollection*)obj;
    return (CsValueSlice){ .ptr = &my->val1, .len = 2 };
}
```

### `finalizer`
A type finalizer is optional since the memory and children of an instance will be freed automatically by ARC.
However, it can be useful to perform additional cleanup tasks for instances that contain external resources.
```c
void myCollectionFinalizer(CsVM* vm, void* obj) {
    printf("MyCollection finalizer was called.\n");
}
```

# Memory.

* [ARC.](#arc)
  * [Reference counting.](#reference-counting)
  * [Optimizations.](#optimizations)
  * [Closures.](#closures-1)
  * [Fibers.](#fibers-1)
* [Heap.](#heap)
  * [Weak references.](#weak-references)
  * [Cycle detection.](#cycle-detection)

[^top](#table-of-contents)

Cyber provides memory safety by default.

## ARC.
Cyber uses ARC or automatic reference counting to manage memory.
ARC is deterministic and has less overhead compared to a tracing garbage collector. Reference counting distributes memory management, which reduces GC pauses and makes ARC suitable for realtime applications. One common issue in ARC implementations is reference cycles which Cyber addresses with [Weak References](#weak-references) and it's very own [Cycle Detection](#cycle-detection).

### Reference counting.
In Cyber, there are [primitive and object](#basic-types) values. Primitives don't need any memory management, since they are copied by value and no heap allocation is required (with the exception of primitives being captured by a [closure](#closures-1). 

Objects are managed by ARC. Each object has its own reference counter. Upon creating a new object, it receives a reference count of 1. When the object is copied, it's **retained** and the reference count increments by 1. When an object value is removed from it's parent or is no longer reachable in the current stack frame, it is **released** and the reference count decrements by 1.

Once the reference count reaches 0 the object begins its destruction procedure. First, child references are released thereby decrementing their reference counts by 1. If the object is a host object, it will invoke its `finalizer` function. Afterwards, the object is freed from memory.

### Optimizations.
The compiler can reduce the number of retain/release ops since it can infer value types even though they are dynamically typed to the user. Arguments passed to functions are only retained depending on the analysis from the callsite.

### Closures.
When primitive variables are captured by a [closure](#closures), they are boxed and allocated on the heap. This means they are managed by ARC and cleaned up when there are no more references to them.

### Fibers.
[Fibers](#fibers) are freed by ARC just like any other object. Once there are no references to the fiber, it begins to release it's child references by unwinding it's call stack.

## Heap.
Many object types in Cyber are small enough to be at or under 40 bytes. To take advantage of this, Cyber can reserve object pools to quickly allocate and free these small objects with very little bookkeeping. Bigger objects are allocated and managed by `mimalloc` which has proven to be a fast and reliable general-purpose heap allocator.

## Weak references.
> _Planned Feature_

## Cycle detection.
The cycle detector is also considered a GC and frees abandoned objects managed by ARC. Although weak references can remove cycles altogether, Cyber does not force you to use them and provides a manual GC as a one-time catch all solution.
> _Incomplete Feature: Only the main fiber stack is cleaned up at the moment._

To invoke the GC, call the builtin function: `performGC`.
```cy
func foo():
    -- Create a reference cycle.
    var a = []
    var b = []
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

# Backends.

* [JIT.](#jit)
* [AOT.](#aot)

[^top](#table-of-contents)

## JIT.
Cyber's just-in-time compiler is incomplete and unstable. To run your script with JIT enabled:
```bash
cyber -jit &lt;script&gt;
```

The JIT compiler is just as fast as the bytecode generation so when it's enabled, the entire script is compiled from the start.

## AOT
Work on the ahead-of-time compiler has not begun.