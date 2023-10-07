---
title: "Data Types"
weight: 2
---

# Data Types.
In Cyber, there are primitive types and object types. Primitives are copied around by value and don't need additional heap memory or reference counts. Primitives include [Booleans](#booleans), [Floats](#floats), [Integers](#integers), [Enums](#enums), [Symbols](#symbols), [Errors]({{<relref "/docs/toc/errors">}}), [Static Strings](#strings), and the `none` value. Object types include [Lists](#lists), [Tuples](#tuples), [Maps](#maps), [Strings](#strings), [Custom Objects](#objects), [Lambdas]({{<relref "/docs/toc/functions#lambdas">}}), [Fibers]({{<relref "/docs/toc/concurrency#fibers">}}), [Errors with payloads]({{<relref "/docs/toc/errors">}}), [Pointers]({{<relref "/docs/toc/ffi#pointers">}}), and several internal object types.

The `none` value represents an empty value. This is similar to null in other languages.

## Booleans.
Booleans can be `true` or `false`. See [`type boolean`]({{<relref "/docs/toc/modules#type-boolean">}}).
```cy
var a = true
if a:
    print 'a is true'
```
When other value types are coerced to the boolean type, the truthy value is determined as follows.
- The `none` value is `false`.
- Other objects and values are always `true`.

## Numbers.

### Integers.
`int` is the default integer type. It has 48-bits and can represent integers in the range -(2{{<sup "47">}}) to 2{{<sup "47">}}-1. See [`type int`]({{<relref "/docs/toc/modules#type-int">}}).

When a numeric literal is used and the type can not be inferred, it will default to the `int` type:
```cy
var a = 123
```

Integer notations always produce a `int` value:
```cy
var a = 0xFF     -- hex.
a = 0o17         -- octal.
a = 0b1010       -- binary.
a = 0u'🐶'       -- UTF-8 rune.
```

Arbitrary values can be converted to a `int` using the type as a function.
```cy
var a = '123'
var b = int(a) 
```

In addition to arithmetic operations, integers can also perform [bitwise operations]({{<relref "/docs/toc/syntax#bitwise-operators">}}).

### Floats.
`float` is the default floating point type. It has a (IEEE 754) 64-bit floating point format. See [`type float`]({{<relref "/docs/toc/modules#type-float">}}).

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

### Big Numbers.
> _Planned Feature_

## Strings.
The `string` type represents a sequence of UTF-8 codepoints, also known as `runes`. Each rune is stored internally as 1-4 bytes and can be represented as an `int`. See [`type string`]({{<relref "/docs/toc/modules#type-string-trait">}}).

Under the hood, Cyber implements 5 different internal string types to optimize string operations.

Strings are **immutable**, so operations that do string manipulation return a new string. By default, small strings are interned to reduce memory footprint.

To mutate an existing string, use the [StringBuffer](#string-buffer).
> _Planned Feature_

A string is always UTF-8 validated. [rawstrings](#rawstring) outperform strings but you'll have to validate them and take care of indexing yourself.

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
| -- | -- | -- |
| \a | 0x07 | Terminal bell. |
| \b | 0x08 | Backspace. |
| \e | 0x1b | Escape character. |
| \n | 0x0a | Line feed character. |
| \r | 0x0d | Carriage return character. |
| \t | 0x09 | Horizontal tab character. |

The boundary of each line can be set with a vertical line character. This makes it easier to see the whitespace.
> _Planned Feature_
```cy
var poem = "line a
       |  two spaces from the left
       |     indented further"
```

Using the index operator will return the UTF-8 rune at the given index as a slice. This is equivalent to calling the method `sliceAt()`.
```cy
var str = 'abcd'
print str[1]     -- "b"
print str[-1]    -- "d"
```

Using the slice index operator will return a view of the string at the given start and end (exclusive) indexes. The start index defaults to 0 and the end index defaults to the string's length.
```cy
var str = 'abcxyz'
var sub = str[0..3]
print sub        -- "abc"
print str[..5]   -- "abcxy"
print str[1..]   -- "bcxyz"

-- One way to use slices is to continue a string operation.
str = 'abcabcabc'
var i = str.findRune(0u'c')
print(i)                            -- "2"
i += 1
print(i + str[i..].findRune(0u'c'))  -- "5"
```

### String Interpolation.

You can embed expressions into string templates using braces.
```cy
var name = 'Bob'
var points = 123
var str = 'Scoreboard: {name} {points}'
```

Escape braces with a backslash.
```cy
var points = 123
var str = 'Scoreboard: \{ Bob \} {points}'
```
String templates can not contain nested string templates.

### rawstring.
A `rawstring` does not automatically validate the string and is indexed by bytes and not UTF-8 runes. See [`type rawstring`]({{<relref "/docs/toc/modules#type-rawstring-trait">}}).

Using the index operator will return the UTF-8 rune starting at the given byte index as a slice. If the index does not begin a valid UTF-8 rune, `error.InvalidRune` is returned. This is equivalent to calling the method `sliceAt()`.
```cy
var str = rawstring('abcd').insertByte(1, 255)
print str[0]     -- "a"
print str[1]     -- error.InvalidRune
print str[-1]    -- "d"
```

## Lists.
Lists are a builtin type that holds an ordered collection of elements. Lists grow or shrink as you insert or remove elements. See [`type List`]({{<relref "/docs/toc/modules#type-list">}}).
```cy
-- Construct a new list.
var list = [1, 2, 3]

-- The first element of the list starts at index 0.
print list[0]    -- Prints '1'

-- Using a negative index starts at the back of the list.
print list[-1]   -- Prints '3'
```

Lists can be sliced with the range `..` clause. The sliced list becomes a new list that you can modify without affecting the original list. The end index is non-inclusive. Negative start or end values count from the end of the list.
```cy
var list = [ 1, 2, 3, 4, 5 ]
list[0..0]  -- []          Empty list.
list[0..3]  -- [ 1, 2, 3 ] From start to end index.
list[3..]   -- [ 4, 5 ]    From start index to end of list. 
list[..3]   -- [ 1, 2, 3 ] From start of list to end index.
list[2..+2] -- [ 3, 4 ]    From start index to start index + amount.
```

List operations.
```cy
var list = [234]
-- Append a value.
list.append 123
print list[-1]     -- Prints '123'

-- Inserting a value at an index.
list.insert(1, 345)

-- Get the length.
print list.len()  -- Prints '2'

-- Sort the list in place.
list.sort((a, b) => a < b)

-- Iterating a list.
for list each it:
    print it

-- Remove an element at a specific index.
list.remove(1)
```

## Tuples.
> _Incomplete: Tuples can only be created from @host funcs at the moment._

## Maps.
Maps are a builtin type that store key value pairs in dictionaries. See [`type Map`]({{<relref "/docs/toc/modules#type-map">}}).
```cy
var map = { a: 123, b: () => 5 }
print map['a']

-- You can also access the map using an access expression.
print map.a

-- Map entries can be separated by the new line.
map = {
    foo: 1
    bar: 2
}
```
Entries can also follow a `{}:` block.
This gives structure to the entries and has
the added benefit of allowing multi-line lambdas.
> _Planned Feature_
```cy
var colors = {}:
    red: 0xFF0000
    green: 0x00FF00
    blue: 0x0000FF
    dump func (c):
        print c.red
        print c.green
        print c.blue

    -- Nested map.
    darker {}: 
        red: 0xAA0000
        green: 0x00AA00
        blue: 0x0000AA
```
Map operations.
```cy
var map = {}
-- Set a key value pair.
map[123] = 234

-- Get the size of the map.
print map.size()

-- Remove an entry by key.
map.remove 123

-- Iterating a list.
for map each [val, key]:
    print '{key} -> {value}'
```

## Objects.
Any value that isn't a primitive is an object. You can declare your own object types using the `type object` declaration.
Object types are similar to structs and classes in other languages.
Unlike classes, there is no concept of inheritance at the language level.

### Fields.
Fields must be declared at the top of the `type object` block:
```cy
type Node object:
    value
    next

var node = Node{ value: 123, next: none }
print node.value       -- Prints "123"
```
New instances of an object template are created using the type name and braces that surround the initial field values. These fields were declared with the `dynamic` type. See how to declare [typed fields]({{<relref "/docs/toc/type-system#object-types">}}).

### Methods.
Methods allow invoking a function using the `.` operator after an object instance. Since the behavior of methods is closely tied to object instances, they are declared with the `meth` keyword:
```cy
type Node object:
    value
    next

    meth inc(n):
        value += n

    meth incAndPrint():
        inc(321)
        print value

var node = Node{ value: 123, next: none }
n.incAndPrint()         -- Prints "444"
```
Type members can be implicitly referenced inside the method.
> _Incomplete: Only the type's fields can be referenced this way._

To reference members explicitly inside a method, use the builtin `self`:
```cy
type Node object:
    value

    meth double():
        return self.value * 2
```

### Functions.
Regular functions are invoked from the type's namespace:
```cy
type Node object:
    value
    next

    -- A function.
    func create():
        return Node{ value: 123, next: none }

var n = Node.create()
```

## Enums.
A new enum type can be declared with the `type enum` declaration.
An enum value can only be one of the unique symbols declared in the enum type.
By default, the symbols generate unique ids starting from 0.
```cy
type Fruit enum:
    apple
    orange
    banana
    kiwi

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

## Symbols.
Symbol literals begin with `.`, followed by an identifier. They have their own global unique id.
```cy
var currency = .usd
print(currency == .usd)   -- 'true'
print int(currency)       -- '123' or some arbitrary id.
```