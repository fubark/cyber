# Table of Contents.
- [Introduction.](#introduction)
- [Syntax.](#syntax)
- [Data Types.](#data-types)

<!--TOC-END-->

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
    print 'Hello, $(w)!'
```

# Syntax.
Cyber's syntax is concise and easy to read.

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
* [Operators.](#operators)
  * [Arithmetic operators.](#arithmetic-operators)
  * [Comparison operators.](#comparison-operators)
  * [Logic operators.](#logic-operators)
  * [Bitwise operators.](#bitwise-operators)
  * [Operator overloading.](#operator-overloading)
* [Comments.](#comments)
* [CYON.](#cyon)

[top](#table-of-contents)

## Statements.
A statement ends with the new line.
```cy
-- An assignment statement.
var a = 123
```
[parent](#syntax)

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
var items = [10, 20, 30]
for items -> it:
    if it == 20:
        print it
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
[parent](#syntax)

## Variables.
Cyber supports dynamic and static typing. If you're used to a dynamic language such as Python, JavaScript, or Lua, use `my` to declare your variables and object fields. If you're used to a static language then use `var` instead. The documentation will use static typing by default.

[parent](#syntax)

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

[parent](#syntax)

### Dynamic variables.
Dynamically typed variables are easier to work with and there is no friction when using them. They are declared using the `my` keyword:
```cy
my a = 123
```
To understand more about dynamically and statically typed code, see [Type System](#type-system).

[parent](#syntax)

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

[parent](#syntax)

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
In the case of [imported](#importing) variables, the order of the import would affect this order.
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
For this use case, you can use a declaration block. *Planned Feature*
```cy
var Root.myImage =:
    var img = loadImage('me.png')
    img.resize(100, 100)
    img.filter(.blur, 5)
    break img
```
The final resulting value that is assigned to the static variable is provided by a `break` statement. If a `break` statement is not provided, `none` is assigned instead.

[parent](#syntax)

## Reserved identifiers.

### Keywords.
There are `25` general keywords. This list categorizes them:

- [Control Flow](#control-flow): `if` `else` `switch` `case` `while` `for` `break` `continue` `pass`
- [Operators](#operators): `or` `and` `not`
- [Variables](#variables): `var` `my`
- [Functions](#functions): `func` `return`
- [Coroutines](#fibers): `coinit` `coyield`, `coresume`
- [Data Types](#data-types): `type` `as`
- [Error Handling](#errors): `try` `catch` `throw`
- [Modules](#modules): `import`

[parent](#syntax)

### Contextual keywords.
These keywords only have meaning in a certain context.
- [Methods](#methods): `self`, `Self`
- [Catching Errors](#caught-variable): `caught`
- [Object Type](#objects): `object`
- [Enum Type](#enums): `enum`
- [Function Throws](#throws-specifier): `throws`

[parent](#syntax)

### Symbols.
- [Modules](#modules): `Root`

[parent](#syntax)

### Literals.
- [Booleans](#booleans): `true` `false`
- [Error Values](#error-value): `error`
- [None](#none): `none`

[parent](#syntax)

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

[parent](#syntax)

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

[parent](#syntax)

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

[parent](#syntax)

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

[parent](#syntax)

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

[parent](#syntax)

### Operator Overloading.
See [Operator Overloading](#operator-overloading) in Metaprogramming.

[parent](#syntax)

## Comments.
A single line comment starts with two hyphens and ends at the end of the line.
```cy
-- This is a comment.

var a = 123   -- This is a comment on the same line as a statement.
```
There will be multi-line comments in Cyber but the syntax has not been determined.

[parent](#syntax)

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

[parent](#syntax)

# Data Types.
In Cyber, there are primitive types and object types. Primitives are copied around by value and don't need additional heap memory or reference counts.

Primitives include [Booleans](#booleans), [Floats](#floats), [Integers](#integers), [Enums](#enums), [Symbols](#symbols-1), [Error Symbols](#errors), and the `none` value.

Object types include [Lists](#lists), [Tuples](#tuples), [Maps](#maps), [Strings](#strings), [Arrays](#arrays), [User Objects](#objects), [Lambdas](#lambdas), [Fibers](#fibers), [Enums with payloads](#enums), [Pointers](#pointers), and several internal object types.

<table><tr>
<td valign="top">

* [None.](#none)
* [Booleans.](#booleans)
* [Numbers.](#numbers)
  * [Integers.](#integers)
  * [Floats.](#floats)
  * [Big Numbers.](#big-numbers)
* [Strings.](#strings)
  * [String interpolation.](#string-interpolation)
  * [String formatting.](#string-formatting)
* [Arrays.](#arrays)
* [Bracket literals.](#bracket-literals)
* [Lists.](#lists)
* [Tuples.](#tuples)
</td>
<td valign="top">

* [Maps.](#maps)
  * [Create map.](#create-map)
  * [Empty map.](#empty-map)
  * [Map indexing.](#map-indexing)
  * [Map operations.](#map-operations)
  * [Map block.](#map-block)
* [Objects.](#objects)
  * [Fields.](#fields)
  * [Instantiation.](#instantiation)
  * [Methods.](#methods)
  * [`self` variable.](#self-variable)
  * [Type functions.](#type-functions)
  * [Type variables.](#type-variables)
* [Enums.](#enums)
* [Symbols.](#symbols-1)
</td>
</tr></table>

## None.
The `none` value represents an empty value. This is similar to null in other languages.

[parent](#data-types)

## Booleans.
Booleans can be `true` or `false`. See [`type bool`](#type-bool).
```cy
var a = true
if a:
    print 'a is true'
```
When other value types are coerced to the bool type, the truthy value is determined as follows.
- The `none` value is `false`.
- Other objects and values are always `true`.

[parent](#data-types)

## Numbers.

### Integers.
`int` is the default integer type. It has 48-bits and can represent integers in the range -(2<sup>47</sup>) to 2<sup>47</sup>-1. See [`type int`](#type-int).

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

[parent](#data-types)

### Floats.
`float` is the default floating point type. It has a (IEEE 754) 64-bit floating point format. See [`type float`](#type-float).

Although a `float` represents a decimal number, it can also represent integers between -(2{{<sup "53">}}-1) and (2{{<sup "53">}}-1). Any integers beyond the safe integer range is not guaranteed to have a unique representation.

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

[parent](#data-types)

### Big Numbers.
> _Planned Feature_

[parent](#data-types)

## Strings.
The `string` type represents a sequence of UTF-8 codepoints, also known as `runes`. Each rune is stored internally as 1-4 bytes and can be represented as an `int`. See [`type string`](#type-string-trait).

Under the hood, there are multiple string implementations to make operations faster by default.

Strings are **immutable**, so operations that do string manipulation return a new string. By default, small strings are interned to reduce memory footprint.

To mutate an existing string, use [MutString](#mutstring). *Planned Feature*

A string is always UTF-8 validated. Using an [Array](#array) to represent raw bytes of a string is faster but you'll have to validate them and take care of indexing.

A single line string literal is surrounded in single quotes.
```cy
var apple = 'a fruit'
```

You can escape the single quote inside the literal or use double quotes.
```cy
var apple = 'Bob\'s fruit'
apple = "Bob's fruit"
```

Concatenate two strings together with the `+` operator or the method `concat`. 
```cy
var res = 'abc' + 'xyz'
res = res.concat('end')
```

Strings are UTF-8 encoded.
```cy
var str = 'abc🦊xyz🐶'
```

Use double quotes to surround a multi-line string.
```cy
var str = "line a
line b
line c"
```

You can escape double quotes inside the literal or use triple quotes.
```cy
var str = "line a
line \"b\"
line c"

-- Using triple quotes.
str = '''line a
line "b"
line c
'''
```

The following escape sequences are supported:

| Escape Sequence | Code | Description |
| --- | --- | --- |
| \a | 0x07 | Terminal bell. |
| \b | 0x08 | Backspace. |
| \e | 0x1b | Escape character. |
| \n | 0x0a | Line feed character. |
| \r | 0x0d | Carriage return character. |
| \t | 0x09 | Horizontal tab character. |

The boundary of each line can be set with a vertical line character. This makes it easier to see the whitespace.
*Planned Feature*
```cy
var poem = "line a
       |  two spaces from the left
       |     indented further"
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

[parent](#data-types)

### String Interpolation.
Expressions can be embedded into string templates with `$()`:
```cy
var name = 'Bob'
var points = 123
var str = 'Scoreboard: $(name) $(points)'
```
String templates can not contain nested string templates.

[parent](#data-types)

### String formatting.
Values that can be formatted into a string will have a `fmt` method:
```cy
var file = os.openFile('data.bin', .read)
var bytes = file.readToEnd()

-- Dump contents in hex.
print '$(bytes.fmt(.x))' 
```

[parent](#data-types)

## Arrays.
An `array` is an immutable sequence of bytes. It can be used to represent strings but it won't automatically validate their encoding and indexing returns the n'th byte rather than a UTF-8 rune. See [`type array`](#type-array).

```cy
var a = array('abcd')
a = a.insertByte(1, 255)
print a[0]     -- "97"
print a[1]     -- "255"
```

[parent](#data-types)

## Bracket literals.
Bracket literals are delimited with brackets `[]`. They are used to initialize Lists, Maps, and Objects:
```cy
var list = [1, 2, 3]
var map = [ a: 123, b: 234 ]
var obj = [MyObject a: 123, b: 234]
```

[parent](#data-types)

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

[parent](#data-types)

## Tuples.
> _Incomplete: Tuples can only be created from @host funcs at the moment._

[parent](#data-types)

## Maps.
Maps are a builtin type that store key value pairs in dictionaries. See [`type Map`](#type-map).

[parent](#data-types)

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

[parent](#data-types)

### Empty map.
The empty map is initialized using `[:]`:
```cy
var empty = [:]
```

[parent](#data-types)

### Map indexing.
Get a value from the map using the index operator:
```cy
print map['a']
```

Maps can be accessed with the `.` dot operator as well:
```cy
print map.a
```

[parent](#data-types)

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
    print '$(key) -> $(val)'
```

[parent](#data-types)

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

[parent](#data-types)

## Objects.
Any value that isn't a primitive is an object. You can declare your own object types using the `type object` declaration.
Object types are similar to structs and classes in other languages.
Unlike classes, there is no concept of inheritance at the language level.

[parent](#data-types)

### Fields.
Fields must be declared at the top of the `type object` block using `var` or `my`:
```cy
type Node object:
    var value int
    var next  any
```
When fields are declared with `my` instead, they become dynamically typed.

[parent](#data-types)

### Instantiation.
New object instances are created using a leading type name and the field values in a collection literal:
```cy
var node = [Node value: 123, next: none]
print node.value       -- Prints "123"
```

[parent](#data-types)

### Methods.
Methods allow invoking a function on an object instance using the `.` operator:
```cy
type Node object:
    var value int
    var next  any

    func inc(n):
        value += n

    func incAndPrint():
        self.inc(321)
        print value

var n = [Node value: 123, next: none]
n.incAndPrint()         -- Prints "444"
```

Methods can also be declared outside of the type declaration. To distinguish a method from a type function, `self` must be provided as a parameter:
```cy
func Node.getNext(self):
    return self.next
```

[parent](#data-types)

### `self` variable.
Type members can be implicitly referenced inside the method. *Incomplete: Only the type's fields can be referenced this way.*

To reference members explicitly inside a method, use the builtin `self`:
```cy
type Node object:
    var value int
    var next  any

    func double():
        return self.value * 2
```

[parent](#data-types)

### Type functions.
Type functions are declared outside of the `type` block with an explicit namespace path:
```cy
type Node object:
    var value int
    var next  any

-- Declare namespace function inside `Node`.
func Node.new():
    return [Node value: 123, next: none]

var n = Node.new()
```

[parent](#data-types)

### Type variables.
Similarily, type variables are declared outside of the `type` block:
```cy
-- Declare inside the `Node` namespace.
var Node.DefaultValue = 100

print Node.DefaultValue    -- Prints "100"
```

[parent](#data-types)

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

[parent](#data-types)

## Symbols.
Symbol literals begin with `.`, followed by an identifier. They have their own global unique id.
```cy
var currency = .usd
print(currency == .usd)   -- 'true'
print int(currency)       -- '123' or some arbitrary id.
```

[parent](#data-types)
