# Cyber v0.1 Docs

### Table of Contents
<table>
<tr>
<td style="vertical-align: top;">

<!-- no toc -->
- [Overview](#overview)
    - [Hello World](#hello-world)
    - [Statements](#statements)
    - [Variables](#variables)
    - [Keywords](#keywords)
    - [Operators](#operators)
    - [Comments](#comments)
- [Data Types](#data-types)
    - [Booleans](#booleans)
    - [Numbers](#numbers)
    - [Strings](#strings) ([type](#object-string))
        - [String Interpolation](#string-interpolation)
        - [rawstring](#raw-string) ([type](#object-rawstring))
    - [Lists](#lists) ([type](#object-list))
    - [Maps](#maps) ([type](#object-map))
    - [Objects](#objects)
    - [Tags](#tags)
- [Control Flow](#control-flow)
    - [Branching](#branching)
    - [Iterations](#iterations)
    - [Matching](#matching)
    - Deferred Execution
- [Functions](#functions)
    - [Static Functions](#static-functions)
        - [Function Overloading](#function-overloading)
    - [Lambdas](#lambdas)
    - [Closures](#closures)
    - [Function Calls](#function-calls)

</td>
<td style="vertical-align: top;">

<!-- no toc -->
- [Modules](#modules)
    - [Importing](#importing)
    - [Exporting](#exporting)
    - [Builtins](#builtin-modules)
        - [core](#core-module)
        - [math](#math-module)
        - [os](#os-module)
        - [test](#test-module)
- [FFI](#ffi)
    - [CFunc](#cfunc)
    - [CStruct](#cstruct)
- [Error Handling](#error-handling)
- [Concurrency](#concurrency)
    - [Fibers](#fibers)
    - Async
    - Multi-thread
- [Metaprogramming](#metaprogramming)
    - Runtime Eval
    - [Compile Time](#compile-time)
    - Generics
- Embedding
- Gradual Typing
- Memory
    - ARC
    - Heap
    - Weak Refs
    - Cycle Detection
- [CYON](#cyon)

</td>
</tr>
</table>

### Overview.
Cyber is a fast, efficient, and concurrent scripting language. To learn more about Cyber itself visit the website at [cyberscript.dev](https://cyberscript.dev).

Cyber is easy to learn. These docs provide a reference manual for the language. You can read it in order or jump around using the navigation.

[To Top.](#table-of-contents)

### Hello World.
```text
worlds = ['World', '世界', 'दुनिया', 'mundo']
for worlds each w:
    print 'Hello, {w}!'
```

[To Top.](#table-of-contents)

### Statements.
In Cyber, a statement ends with the new line.
A statement block is not surrounded by delimiters like braces.
Instead, the beginning of a block starts with a colon.
This is intended to make a clear distinction between expressions and structured statements.
```text
-- This is a statement.
a = 123

-- This statement begins a new block.
if true:
    a = 234
```
The first statement in a block must be indented further than the block declaration. Spaces or tabs can be used for indentation but not both. The rest of the statements in the block must follow the same indentation. When the next statement recedes from this indentation the block ends.
```text
for items each it:
    if it == 20:
        print it
        print it
    print it      -- This is the first statement outside of the `if` block.
```
Single line blocks allows you to have one statement on the same line as the block statement.
```text
-- A single line block.
if true: print 123

if true: print 123
    -- This is an indentation error since the single line block is already consumed.
    print 234
```

[To Top.](#table-of-contents)

### Variables.
In Cyber, there are local variables and static variables.

[To Top.](#table-of-contents)

### Local Variables.
Local variables exist until the end of their scope.
They are set using the assignment statement.
```text
a = 123
```

Function blocks and the main block have their own local variable scope.
Variable assignments prefer to set the variable in the current block. If there is no such variable in the current block, it will create a new local variable.
```text
a = 123
func foo():
    -- A new local `a` inside `foo`.
    a = 234
foo()
print a      -- '123'
```

If the assignment was intended for a parent local variable, the `capture` keyword is used.
```text
a = 123
func foo():
    capture a = 234
foo()
print a      -- '234'
```

Once declared with `capture`, any subsequent assignments will also set the parent local variable.
```text
a = 123
func foo():
    capture a = 234
    a = 345
foo()
print a      -- '345'
```

Control flow constructs such as `if` or `for` are considered sub-blocks and share the same variable scope as the block they are in.
```text
a = 123
if true:
    a = 234
print a      -- '234'
```

Referencing a variable that doesn't exist in the current block, will find the first variable above the current block.
```text
a = 123
func foo():
    print a
foo()        -- '123'
```

[To Top.](#table-of-contents)

### Static Variables.
Unlike local variables, static variables are always available until the end of the script.
When declared in the main block, they act as global variables and are visible from anywhere in the script without being captured by a closure. 
You can declare static variables with the `var` keyword.
```text
var a = 123
func foo():
    print a     -- '123'
```

Cyber's static variable declaration can be a source of confusion if you're coming from a language that uses `var` as a local variable.
As a rule of thumb, static declarations in Cyber always begin with a keyword that describes what it is: `var`, `func`, `object`, `module`, `import`.
Local variables are declared by the first assignment statement in a block.

Since assignment statements prefer to write to a variable in it's local block, the `static` keyword is used to select a static variable instead.
```text
var a = 123
func foo():
    static a = 234
foo()
print a         -- '234'
```

Static variables can also be exported from the current script. You can read more about exports and [Modules](#modules).
```text
export var a = 123
```

When declared in functions, static variables are initialized once and continue to exist for subsequent function calls.
```text
func add(a):
    var sum = 0
    sum += a
    return sum
print add(5)     -- '5'
print add(5)     -- '10'
```

Since static variable declarations are initialized outside of the normal execution flow, they can not reference any local variables.
```text
a = 123
var b = a     -- Compile error, initializer can not reference a local variable.
```

However, you can reassign any value to them with an assignment statement.
```text
a = 123
var b = 0
b = a         -- Reassigning can reference a local variable.
```

Static variable initializers have a natural order based on when it was encountered by the compiler.
In the case of [imported](#importing) variables, the order of the import would affect this order.
The following would print '123' before '234'
```text
var a = print(123)
var b = print(234)
```

When the initializers reference other static variables, those child references are initialized first in DFS order and supersede the natural ordering. The following initializes `b` before `a`.
```text
var a = b + 321
var b = 123
print a        -- '444'
```

Circular references in initializers are allowed.
When initialization encounters a reference that creates this circular dependency, that reference evaluates to `none` at that moment.
In the following, `a` attempts to initialize first because of its natural ordering. Since `b` is a dependency, it supersedes the natural ordering.
When `b` is found to reference an already visited `a` (causing the circular dependency), it evaluatues to `a`'s current value which is `none`. At the end of initialization, both `a` and `b` have the value `none`.
```text
var a = b
var b = a
```

Sometimes, you may want to initialize a static variable by executing multiple statements in order.
For this use case, you can use a declaration block.
```
var myImage =:
    img = loadImage('me.png')
    img.resize(100, 100)
    img.filter(#blur, 5)
    break img
```
The final resulting value that is set to the static variable is provided by a `break` statement. If a `break` statement is not provided, `none` is used instead.

[To Top.](#table-of-contents)

### Keywords.
There are currently `34` keywords in Cyber. This list categorizes them and shows you when you might need them.

- [Control Flow](#control-flow): `if` `then` `else` `match` `while` `for` `each` `break` `continue` `pass`
- [Operators](#operators): `or` `and` `not` `is`
- [Variables](#variables): `var` `static` `capture` `as`
- [Functions](#functions): `func` `return`
- [Coroutines](#fibers): `coinit` `coyield`, `coresume`
- [Data Types](#data-types): `object` `tagtype` `true` `false` `none`
- [Error Handling](#error-handling): `try` `catch` `recover`
- [Modules](#modules): `import` `export`
- Metaprogramming: `compt`

[To Top.](#table-of-contents)

### Operators.
Cyber supports the following arithmetic operators. To understand how these operators affect the number type, see [Numbers](#numbers).
```text
1 + 2     -- Addition, evaluates to 3.
100 - 10  -- Subtraction, evaluates to 90.
3 * 4     -- Multiplication, evaluates to 12.
20 / 5    -- Division, evaluates to 4.
2 ^ 4     -- Raise to the power, evaluates to 16.
12 % 5    -- Modulus remainder, evaluates to 2.
-(10)     -- Apply negative, evaluates to -10.
```
[To Top.](#table-of-contents)

### Comparison Operators.

Cyber supports the following comparison operators.
A comparison expression always evaluates to a [Boolean](#booleans) value.

The equals operator returns true if the two values are equal. For primitive types, the comparison checks the types and the underlying value. For strings, the underlying bytes are compared for equality. For objects, the comparison checks that the two values reference the same object. The not equals operator returns true if the two values are not equal.
```text
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
```text
a > b    -- `true` if a is greater than b
a >= b   -- `true` if a is greater than or equal to b
a < b    -- `true` if a is less than b
a <= b   -- `true` if a is less than or equal to b
```

[To Top.](#table-of-contents)

### Logic Operators.

The logical operators `and`, `or`, and `not` are supported.

`and` evaluates to `a` if `a` is not truthy. Otherwise, it evaluates to `b`. If `a` is not truthy, the evaluation of `b` is not executed. A number value that isn't 0 is truthy. An object reference is always truthy. The none value is not truthy.
```text
true and true  -- Evaluates to true
123 and 234    -- Evaluates to 234
123 and 0      -- Evaluates to false
```
`or` evaluates to `a` if `a` is truthy. Otherwise, it evaluates to `b`. If `a` is found to be truthy, the evaluation of `b` is not executed.
```text
true or false  -- Evaluates to true
false or true  -- Evaluates to true
false or false -- Evaluates to false
123 or false   -- Evaluates to 123
```

The unary operator `not` performs negation on the boolean value. The unary operator `!` can also be used instead of `not`.
```text
not false     -- Evaluates to true
not true      -- Evaluates to false
not 0         -- Evaluates to true      
not 123       -- Evaluates to false
!false        -- Evaluates to true
!true         -- Evaluates to false
```
[To Top.](#table-of-contents)

### Bitwise Operators.

Cyber supports the following bitwise operators for integer values. Number operands are first converted to a 32-bit integer (two's complement) before applying the operation.
```text
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

[To Top.](#table-of-contents)

### Comments.
A single line comment starts with two hyphens and ends at the end of the line.
```text
-- This is a comment.

a = 123   -- This is a comment on the same line as a statement.
```
There will be multi-line comments in Cyber but the syntax has not been determined.

[To Top.](#table-of-contents)

### Data Types.
In Cyber, there are primitive types and object types. Primitives are copied around by value and don't need additional heap memory or reference counts. Primitives include [Booleans](#booleans), [Numbers](#numbers), Integers, [Tags](#tags), [Tag Literals](#tags), [Errors](#error-handling), [Static Strings](#strings), and the `none` value. Object types include [Lists](#lists), [Maps](#maps), [Strings](#strings), [Custom Objects](#objects), [Lambdas](#lambdas), [Fibers](#fibers), [Errors with payloads](#error-handling), and several internal object types.

The `none` value represents an empty value. This is similar to null in other languages.

[To Top.](#table-of-contents)

### Booleans.
Booleans can be `true` or `false`.
```text
a = true
if a:
    print 'a is true'
```
When other value types are coerced to the boolean type, the truthy value is determined as follows.
- Numbers are `true` if the value is not 0.
- Strings are `true` if the length is greater than 0.
- Other objects are always `true`.
- The `none` value is `false`.

[To Top.](#table-of-contents)

### Numbers.
In Cyber, the `number` is the default number type and has a 64-bit double precision floating point format.

You can still use numbers as integers and perform arithmetic without rounding issues in a 32-bit integer range. The safe integer range is from -(2^53-1) to (2^53-1). Any integers beyond this range is not guaranteed to have a unique representation.

When performing bitwise operations, the number is first converted to an 32-bit integer.

```text
a = 123
b = 2.34567
```

There are other number literal notations you can use.
```text
-- Scientific notation. 
a = 123.0e4

-- Integer notations.
a = 0xFF     -- hex.
a = 0o17     -- octal.
a = 0b1010   -- binary.
```

The `int` type is a 32-bit integer and has limited support and you can only declare them in function param and return types.
```text
func fib(n int) int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)
print(fib(30))
```


Big numbers will be supported in a future version of Cyber.

[To Top.](#table-of-contents)

### Strings.
The `string` type represents a sequence of UTF-8 characters. Under the hood, Cyber implements 6 different internal string types to optimize string operations, but the user just sees them as one type and doesn't need to care about this detail under normal usage.

Strings are **immutable**, so operations that do string manipulation return a new string. By default, small strings are interned to reduce memory footprint. To mutate an existing string, use the [StringBuffer](#string-buffer).

A string is always UTF-8 validated. [rawstrings](#raw-strings) outperform strings but you'll have to validate them and take care of indexing yourself.

A single line string literal is surrounded in single quotes.
```text
apple = 'a fruit'
```

You can escape the single quote inside the literal or use double quotes.
```text
apple = 'Bob\'s fruit'
apple = "Bob's fruit"
```

Strings are UTF-8 encoded.
```text
str = 'abc🦊xyz🐶'
```

Use double quotes to surround a multi-line string.
```text
str = "line a
line b
line c"
```

You can escape double quotes inside the literal or use triple quotes.
```text
str = "line a
line \"b\"
line c"

-- Using triple quotes.
str = '''line a
line "b"
line c
'''
```

The boundary of each line can be set with a vertical line character. This makes it easier to see the whitespace.
```text
poem = "line a
       |  two spaces from the left
       |     indented further"
```

Using the index operator will return the UTF-8 character at the given character index. This is equivalent to calling the method `charAt()`.
```text
str = 'abcd'
print str[1]     -- "b"
print str[-1]    -- "d"
```

Using the slice index operator will return a view of the string at the given start and end (exclusive) indexes. The start index defaults to 0 and the end index defaults to the character length of the string.
```text
str = 'abcxyz'
sub = str[0..3]
print sub        -- "abc"
print str[..5]   -- "abcxy"
print str[1..]   -- "bcxyz"

-- One way to use slices is to continue a string operation.
str = 'abcabcabc'
i = str.indexChar('c')
print(i)                            -- "2"
i += 1
print(i + str[i..].indexChar('c'))  -- "5"
```

[To Top.](#table-of-contents)

#### object string
| Method | Summary |
| ------------- | ----- |
| `append(str string) string` | Deprecated: Use `concat()`. | 
| `charAt(idx number) string` | Returns the UTF-8 character at index `idx` as a single character string.  | 
| `codeAt(idx number) number` | Returns the codepoint of the UTF-8 character at index `idx`. | 
| `concat(str string) string` | Returns a new string that concats this string and `str`. | 
| `endsWith(suffix string) bool` | Returns whether the string ends with `suffix`. | 
| `index(needle string) number?` | Returns the first index of substring `needle` in the string or `none` if not found. | 
| `indexChar(needle string) number?` | Returns the first index of UTF-8 character `needle` in the string or `none` if not found. | 
| `indexCharSet(set string) number?` | Returns the first index of any UTF-8 character in `set` or `none` if not found. | 
| `indexCode(needle number) number?` | Returns the first index of UTF-8 codepoint `needle` in the string or `none` if not found. | 
| `insert(idx number, str string) string` | Returns a new string with `str` inserted at index `idx`. |
| `isAscii() bool` | Returns whether the string contains all ASCII characters. | 
| `len() number` | Returns the number of UTF-8 characters in the string. | 
| `less(str string) bool` | Returns whether this string is lexicographically before `str`. |
| `lower() string` | Returns this string in lowercase. | 
| `replace(needle string, replacement string) string` | Returns a new string with all occurrences of `needle` replaced with `replacement`. | 
| `repeat(n number) string` | Returns a new string with this string repeated `n` times. | 
| `slice(start number, end number) string` | Returns a slice into this string from `start` to `end` (exclusive) indexes. This is equivalent to using the slice index operator `[start..end]`. | 
| `startsWith(prefix string) bool` | Returns whether the string starts with `prefix`. | 
| `upper() string` | Returns this string in uppercase. | 

[To Top.](#table-of-contents)

### String Interpolation.

You can embed expressions into string templates using braces.
```text
name = 'Bob'
points = 123
str = 'Scoreboard: {name} {points}'
```

Escape braces with a backslash.
```text
points = 123
str = 'Scoreboard: \{ Bob \} {points}'
```
String templates can not contain nested string templates.

[To Top.](#table-of-contents)

### rawstring.
A `rawstring` does not automatically validate the string and is indexed by bytes and not UTF-8 characters.

Using the index operator will return the UTF-8 character starting at the given byte index. If the index does not begin a valid UTF-8 character, `error(#InvalidChar)` is returned. This is equivalent to calling the method `charAt()`.
```text
str = rawstring('abcd').insertByte(1, 255)
print str[0]     -- "a"
print str[1]     -- error(#InvalidChar)
print str[-1]    -- "d"
```

#### object rawstring
| Method | Summary |
| ------------- | ----- |
| `append(str string) string` | Deprecated: Use `concat()`. | 
| `byteAt(idx number) number` | Returns the byte value (0-255) at the given index `idx`. | 
| `charAt(idx number) string` | Returns the UTF-8 character at index `idx` as a single character string. If the index does not begin a UTF-8 character, `error(#InvalidChar)` is returned. | 
| `codeAt(idx number) number` | Returns the codepoint of the UTF-8 character at index `idx`. If the index does not begin a UTF-8 character, `error(#InvalidChar)` is returned. | 
| `concat(str string) string` | Returns a new string that concats this string and `str`. | 
| `endsWith(suffix string) bool` | Returns whether the string ends with `suffix`. | 
| `index(needle string) number?` | Returns the first index of substring `needle` in the string or `none` if not found. | 
| `indexChar(needle string) number?` | Returns the first index of UTF-8 character `needle` in the string or `none` if not found. | 
| `indexCharSet(set string) number?` | Returns the first index of any UTF-8 character in `set` or `none` if not found. |
| `indexCode(needle number) number?` | Returns the first index of UTF-8 codepoint `needle` in the string or `none` if not found. | 
| `insert(idx number, str string) string` | Returns a new string with `str` inserted at index `idx`. |
| `insertByte(idx number, byte number) string` | Returns a new string with `byte` inserted at index `idx`. | 
| `isAscii() bool` | Returns whether the string contains all ASCII characters. | 
| `len() number` | Returns the number of bytes in the string. | 
| `less(str rawstring) bool` | Returns whether this rawstring is lexicographically before `str`. |
| `lower() string` | Returns this string in lowercase. | 
| `repeat(n number) rawstring` | Returns a new rawstring with this rawstring repeated `n` times. | 
| `replace(needle string, replacement string) string` | Returns a new string with all occurrences of `needle` replaced with `replacement`. | 
| `slice(start number, end number) rawstring` | Returns a slice into this string from `start` to `end` (exclusive) indexes. This is equivalent to using the slice index operator `[start..end]`. | 
| `startsWith(prefix string) bool` | Returns whether the string starts with `prefix`. | 
| `toString() string` | Deprecated: Use `utf8()`. | 
| `upper() string` | Returns this string in uppercase. | 
| `utf8() string` | Returns a valid UTF-8 string or returns `error(#InvalidChar)`. | 

### Lists.
Lists are a builtin type that holds an ordered collection of elements. Lists grow or shrink as you insert or remove elements.
```text
-- Construct a new list.
list = [1, 2, 3]

-- The first element of the list starts at index 0.
print list[0]    -- Prints '1'

-- Using a negative index starts at the back of the list.
print list[-1]   -- Prints '3'
```

Lists can be sliced with the range `..` clause. The sliced list becomes a new list that you can modify without affecting the original list. The end index is non-inclusive. Negative start or end values count from the end of the list.
```text
list = [ 1, 2, 3, 4, 5 ]
list[0..0]  -- []          Empty list.
list[0..3]  -- [ 1, 2, 3 ] From start to end index.
list[3..]   -- [ 4, 5 ]    From start index to end of list. 
list[..3]   -- [ 1, 2, 3 ] From start of list to end index.
list[2..+2] -- [ 3, 4 ]    From start index to start index + amount.
```

List operations.
```text
list = [234]
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

[To Top.](#table-of-contents)

#### object list
| Method | Summary |
| ------------- | ----- |
| `add(val any) none` | Deprecated: Use `append()`. |
| `append(val any) none` | Appends a value to the end of the list. |
| `concat(val any) none` | Concats the elements of another list to the end of this list. |
| `insert(idx number, val any) none` | Inserts a value at index `idx`. |
| `iterator() Iterator<any>` | Returns a new iterator over the list elements. |
| `joinString(separator any) string` | Returns a new string that joins the elements with `separator`. |
| `len() number` | Returns the number of elements in the list. |
| `pairIterator() PairIterator<number, any>` | Returns a new pair iterator over the list elements. |
| `remove(idx number) none` | Removes an element at index `idx`. |
| `resize(len number) none` | Resizes the list to `len` elements. If the new size is bigger, `none` values are appended to the list. If the new size is smaller, elements at the end of the list are removed. |
| `sort(less func (a, b) bool) none` | Sorts the list with the given `less` function. If element `a` should be ordered before `b`, the function should return `true` otherwise `false`. |

[To Top.](#table-of-contents)

### Maps.
Maps are a builtin type that store key value pairs in dictionaries.
```text
map = { a: 123, b: () => 5 }
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
```text
colors = {}:
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
```text
map = {}
-- Set a key value pair.
map[123] = 234

-- Get the size of the map.
print map.size()

-- Remove an entry by key.
map.remove 123

-- Iterating a list.
for map each val, key:
    print '{key} -> {value}'
```

[To Top.](#table-of-contents)

#### object map
| Method | Summary |
| ------------- | ----- |
| `iterator() Iterator<any>` | Returns a new iterator over the map elements. |
| `pairIterator() PairIterator<number, any>` | Returns a new pair iterator over the map elements. |
| `remove(key any) none` | Removes the element with the given key `key`. |
| `size() number` | Returns the number of key-value pairs in the map. |

[To Top.](#table-of-contents)

### Objects.
Any value that isn't a primitive is an object. You can declare your own object types using the `object` keyword. Object templates are similar to structs and classes in other languages. You can declare members and methods. Unlike classes, there is no concept of inheritance at the language level.
```text
object Node:
    value
    next

node = Node{ value: 123, next: none }
print node.value          -- '123'
```
New instances of an object template are created using the type name and braces that surround the initial member values.
When declaring methods, the first parameter must be `self`. Otherwise, it becomes a function that can only be invoked from the type's namespace.
```text
object Node:
    value
    next

    -- A static function.
    func create():
        return Node{ value: 123, next: none }

    -- A method.
    func dump(self)
        print self.value

n = Node.create()
n.dump()
```

[To Top.](#table-of-contents)

### Tags.
Tags are similar to enums in other languages. A new tag type can be declared with the `tagtype` keyword.
A tag value can only be one of the unique tags declared in its tag type.
By default, the tags have a unique id generated starting from 0.
```text
tagtype Fruit:
    apple
    orange
    banana
    kiwi

fruit = Fruit#kiwi
print fruit          -- '#kiwi'
print number(fruit)  -- '3'
```
When the type of the value is known to be a tag, it can be assigned using a tag literal.
```text
fruit = Fruit#kiwi
fruit = #orange
print(fruit == Fruit#orange)   -- 'true'
```
Tag literals by themselves also have a global unique id. When assigned to a non tag value, it becomes a tag literal value.
```text
tagtype MyColor:
    red
    green
    blue
color = #red                 -- The variable `color` does not become a `MyColor` tag.
print(color == Color#red)    -- 'false'
print(color == #red)         -- 'true'
print number(color)          -- '123' or some arbitrary id.
```

[To Top.](#table-of-contents)

### Control Flow.
Cyber provides the common constructs to branch and enter loops.

[To Top.](#table-of-contents)

### Branching.
Use `if` and `else` statements to branch the execution of your code depending on conditions. The `else` clause can contain a condition which is only evaluated if the previous if or conditional else clause was `false`. 
```text
a = 10
if a == 10:
    print 'a is 10'
else a == 20:
    print 'a is 20'
else:
    print 'neither 10 nor 20'
```
An `if` expression also needs the `then` keyword. Conditional `else` clauses are not allowed in an `if` expression.:
```text
a = 10
str = if a == 10 then 'red' else 'blue'
```
Use `and` and `or` logical operators to combine conditions:
```text
a = 10
if a > 5 and a < 15:
    print 'a is between 5 and 15'
if a == 20 or a == 10: 
    print 'a is 10 or 20'
```
[To Top.](#table-of-contents)

### Iterations.
Infinite and conditional loops start with the `while` keyword. An infinite loop continues to run the code in the block until a `break` or `return` is reached.
When the `while` clause contains a condition, the loop continues to run until the condition is evaluated to `false`.
```text
-- Infinite loop.
while:
    pass

running = true
while running:
    -- Keep looping until `running` is false.
    pass
```
`for` loops can iterate over a range that starts at a number (inclusive) to a target number (exclusive). When the range operator `..` is replaced with `..=`, the target number is inclusive. The range can be given a custom step.
```text
for 0..100 each i:
    print i    -- 0, 1, 2, ... , 99

for 0..100, 10 each i:
    print i    -- 0, 10, 20, ... , 90

for 100..0, 1 each i:
    print i    -- 100, 99, 98, ... , 1

for 100..=0, 1 each i:
    print i    -- 100, 99, 98, ... , 0
```
The `for` clause can iterate over an `Iterable` object. An Iterable type contains an `iterator()` method that returns an `Iterator` object. An Iterator type contains a `next()` method that returns the next value or `none` when finished.
You can iterate lists since they are Iterable.
```text
list = [1, 2, 3, 4, 5]

-- Iterate on values.
for list each n:
    print n
```
When the `as` clause contains two variables, the for loop will iterate a `PairIterable` object. A PairIterable type contains a `pairIterator()` method that returns a `PairIterator` object. A PairIterator type contains a `nextPair()` method that returns two values or `none` on the first value when finished. 
The list object is also a PairIterable and the key is the index of the value in the list.
```text
-- Iterate on values and indexes.
for list each i, n:
    print '{i} -> {n}'

-- Iterate on just indexes.
for list each i, _:
    print i 
```
The `for` clause can also iterate over maps with the same idea.
```text
map = { a: 123, b: 234 }

-- Iterate on values.
for map each v:
    print v

-- Iterate on values and keys.
for map each k, v:
    print '{k} -> {v}'

-- Iterate on just keys.
for map each k, _:
    print k
```
If you have an iterator already, you can use the optional `for` loop to continue executing an expression until it is the `none` value. Adding an `as` clause will save the expression's value to the given variable.
```text
iter = dir.walk()
for iter.next() as entry:
    print entry.name
```

You can exit a loop using `break`.
```text
for 0..10 each i:
    if i == 4:
        break
    print i
-- This loop stops printing once `i` reaches 4.
```
You can skip the rest of the loop and go to the next iteration using `continue`.
```text
for 0..10 each i:
    if i == 4:
        continue
    print i
-- This loop prints 0 through 9 but skips 4.
```
[To Top.](#table-of-contents)

### Matching
Matching is similar to a switch statement. The expression to the right of `match` is evaluated and execution jumps to the declared case with the matching value. Multiple cases can be grouped together using a comma separator. An optional `else` fallback case is executed when no other cases were matched.
```text
val = 1000
match val:
    0..100: print 'at or between 0 and 99'
    100: print 'val is 100'
    200:
        print 'val is 200'
    300, 400:
        print 'combined case'
    else:
        print 'val is {val}'
```

[To Top.](#table-of-contents)

### Functions.
In Cyber, there are first-class functions (or function values) as well as statically defined functions.

[To Top.](#table-of-contents)

### Static Functions.
Static functions are not initally values themselves. They allow function calls to be optimal since they don't need to resolve a dynamic value. A nice feature of Cyber's static functions is they can be used just like a function value.

Static functions are declared with the `func` keyword and must have a name.
```text
import m 'math'

func dist(x0, y0, x1, y1):
    dx = x0-x1
    dy = y0-y1
    return m.sqrt(dx^2 + dy^2)
```
Calling static functions is straightforward. You can also reassign or pass them around as values.
```text
print dist(0, 0, 10, 20)

-- Assigning to a local variable.
bar = dist

-- Passing `dist` as an argument.
func squareDist(dist, size):
    return dist(0, 0, size, size)
print squareDist(dist, 30)
```

Functions can return multiple values.
```text
import {cos, sin} 'math'

func compute(rad):
    return cos(rad), sin(rad)
x, y = compute(pi)
```

[To Top.](#table-of-contents)

### Function Overloading.
Static functions can be overloaded by the number of parameters in its signature.
```text
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

[To Top.](#table-of-contents)


### Lambdas.
Lambdas or function values can be assigned to variables or passed as arguments into other constructs.

When a lambda only returns an expression, it can be declared with a simplified syntax.
```text
-- Passing simple lambda as an argument.
foo(word => toUpper(word))

-- A simple lambda with multiple arguments.
foo((word, prefix) => prefix + toUpper(word))

-- Assigning a simple lambda.
canvas.onUpdate = delta_ms => print delta_ms
```

Lambdas that need a block of statements can be declared with the `func` keyword without a name.
```text
-- Assigning lambda block to a variable.
add = func (a, b):
    return a + b

-- Passing a lambda block as an argument.
canvas.onUpdate():
    ..func (delta_ms):
        print delta_ms
```
Passing a lambda block as a call argument is only possible in a call expression block. To understand how that works, see [Function Calls](#function-calls).

[To Top.](#table-of-contents)

### Closures.
In Cyber, functions can close over local variables in parent blocks.
```text
func add():
    a = 123
    return b => a + b
addTo = add()
addTo(10)         -- Prints '133'
```

[To Top.](#table-of-contents)

### Function Calls.
The straightforward way to call a function is to use parentheses.

```text
d = dist(100, 100, 200, 200)
```

You can call functions with named parameters.
```text
d = dist(x0: 10, x1: 20, y0: 30, y1: 40)
```

The shorthand method for calling functions omits parentheses and commas. This only works for functions that accept parameters:
```text
d = dist 100 100 200 200  -- Calls the function `dist`.

func random():            -- Function with no parameters.
    return 4

r = random                -- Returns the function itself as a value. Does not call the function `random`.
r = random()              -- Calls the function `random`.
```

The top level arguments for the shorthand convention must be separated by whitespace. A string can contain whitespace since it's surrounded by delimiters. 
```text
a = myFunc 'cyber script'
```

The following has a binary expression with spaces inbetween which is not allowed. Removing that whitespace fixes the call expression.

```text
a = myFunc 1 + 2     -- Not allowed.
a = myFunc 1+2       -- Correct.
```

Wrapping arguments in parentheses allows you to keep the whitespace in the sub-expression.
```text
-- This calls the function `myFunc` with 2 arguments.
a = myFunc 'hello' (1 + 2 * 3)

-- Nested function call using the shorthand convention.
a = myFunc 'hello' (otherFunc 1+2 'world')
```

The call expression block continues to add arguments from the block's body. If arguments are omitted from the initial call expression they can be added inside using the `..` syntax. Arguments mapped to named parameters have a key value syntax separated by a `:`. All other arguments are added into a list and passed as the last argument.
```text
foo(123):
    ..func ():
        return 123
    param3: 123
    234
    bar()
    'hello'
```
In the example above, the function `foo` is called with 4 arguments. The first argument `123` is included in the starting call expression. The second argument is a function value inside the call expression block. The third argument is mapped to the param `param3`. Finally, the fourth argument is a list that contains `234`, `bar()`, and `'hello'`. 

[To Top.](#table-of-contents)

### Modules.
Modules contain accessible static symbols under a defined namespace. By default, importing another Cyber script returns its module with exported symbols.

[To Top.](#table-of-contents)

### Importing.
Import declarations create a local alias to the module referenced by the import specifier. The Cyber CLI comes with some builtin modules like `math` and `test`. If the specifier does not refer to a builtin module, it looks for a Cyber script file relative to the current script's directory. An embedder can integrate their own module loader.
```text
import t 'test'
try t.eq(123, 123)

-- Imports are static declarations so they can be anywhere in the script.
import m 'math'
print m.cos(0)

-- Loading another Cyber script.
import foo 'bar.cy'
print foo.myFunc()
print foo.myVar
```
A Cyber script that is imported doesn't evaluate its main block. Only static declarations are effectively loaded. If there is code in the main block, it will skip evaluation. In the following, only the `print` statement in the `main.cy` is evaluated.
```text
-- main.cy
import a 'foo.cy'
print a.foo

-- foo.cy
import 'bar.cy'
export var foo = 123
print foo         -- Statement is ignored.

-- bar.cy
export var bar = 321
print bar         -- Statement is ignored.
```
You can have circular imports in Cyber. In the following example, `main.cy` and `foo.cy` import each other without any problems.
```text
-- main.cy
import foo 'foo.cy'

export func printB():
    foo.printC()

foo.printA()

-- foo.cy
import main 'main.cy'

export func printA():
    main.printB()

export func printC():
    print 'done'
```
Static variable declarations from imports can have circular references. Read more about this in [Static Variables](#static-variables).

Modules can also be destructured using the following syntax:
```text
import { cos, pi } 'math'
print cos(pi)
```

[To Top.](#table-of-contents)

### Exporting.
Use the `export` prefix in front of static declarations to indicate that it should be exported when the script's module is loaded.
```text
export func foo():
    print 123

export var bar = 234
```

[To Top.](#table-of-contents)

### Builtin Modules.

Cyber currently contains the builtin modules:
- [core](#core-module): Cyber related functions and commonly used utilities.
- [math](#math-module): Math constants and functions.
- [os](#os-module): System level functions.
- [test](#test-module): Utilities for testing.

[To Top.](#table-of-contents)

### Core Module.
The core module contains functions related to Cyber and common utilities. It is automatically imported into each script's namespace. 

Sample usage:
```text
print 'hello'
contents = readFile 'foo.txt'
print contents
```

| Function | Summary |
| ------------- | ----- |
| `arrayFill(val any, n number) list` | Creates a list with initial capacity of `n` and values set to `val`. If the value is an object, it is shallow copied `n` times. | 
| `asciiCode(val any) number` | Converts the first character of a string to an ASCII code number. | 
| `bool(val any) bool` | Converts a value to either `true` or `false`. | 
| `bindLib(path string, decls [](CFunc\|CStruct)) map` | Calls `bindLib(path, decls, {})`. | 
| `bindLib(path string, decls [](CFunc\|CStruct), config: BindLibConfig) map` | Creates an FFI binding to a dynamic library and it's symbols. By default, an anonymous object is returned with the C-functions binded as the object's methods. If `config` contains `genMap: true`, a map is returned instead with C-functions binded as function values. | 
| `char(val any) number` | Deprecated: Use `asciiCode` instead. |
| `copy(val any) any` | Copies a primitive value or creates a shallow copy of an object value. | 
| `execCmd(args []string) map{ out, err, exited }` | Runs a shell command and returns the stdout/stderr. | 
| `exit(status number) noreturn` | Exits the program with a status code. | 
| `error(any) error` | Create an error from a tag or tag literal. | 
| `evalJS(val string) none` | Evals JS from the host environment. This is only available in a web WASM build of Cyber. | 
| `fetchUrl(url string) rawstring` | Uses previously installed `curl` to fetch the contents at a URL. If `curl` does not exist, `error(#FileNotFound)` is returned. Cyber has not included an http/tls library yet. | 
| `getInput() rawstring` | Reads stdin until a new line is reached. This is intended to read user input from the command line. For bulk reads from stdin, use `os.stdin`. | 
| `int(val any) int` | Converts a value to an 32-bit integer. | 
| `must(val any) any \| noreturn` | If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned. | 
| `number(val any) number` | Converts a value to a number. | 
| `opaque(val any) opaque` | Converts a value to an opaque pointer value. This is usually used with FFI. | 
| `panic(e taglit) noreturn` | Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors](#unexpected-errors). |
| `parseCyon(cyon string) any` | Parses a CYON string into a value. | 
| `print(s string) none` | Prints a value as a string to stdout. The new line is also printed. | 
| `prints(s string) none` | Prints a value as a string to stdout. | 
| `rawstring(str string) rawstring` | Converts a string to a `rawstring`. | 
| `readAll() rawstring` | Reads stdin to the EOF as a `rawstring`. | 
| `readFile(path string) rawstring` | Reads the file contents into a `rawstring` value. | 
| `readLine() rawstring` | Deprecated: Use `getInput` instead. |
| `string(val any) string` | Converts a value to a string. | 
| `valtag(any) #taglit` | Returns the value's type as a tag literal. |
| `writeFile(path string, contents string) none` | Writes a string value to a file. | 

[To Top.](#table-of-contents)

### Math Module.
The math module contains commonly used math constants and functions.

Sample usage:
```text
import m 'math'

r = 10
print(m.pi * r^2)
```
| Variable | Type | Summary |
| ------------- | ------------- | ----- |
| e | number | Euler's number and the base of natural logarithms; approximately 2.718. |
| inf | number | Infinity. |
| log10e | number | Base-10 logarithm of E; approximately 0.434. |
| log2e | number | Base-2 logarithm of E; approximately 1.443. |
| ln10 | number | Natural logarithm of 10; approximately 2.303. |
| ln2 | number | Natural logarithm of 2; approximately 0.693. |
| nan | number | Not a number. Note that nan == nan, however, if a nan came from an arithmetic operation, the comparison is undefined (it may be true or false, so it is not reliable). |
| neginf | number | Negative infinity. |
| pi | number | Ratio of a circle's circumference to its diameter; approximately 3.14159. |
| sqrt1_2 | number | Square root of ½; approximately 0.707. |
| sqrt2 | number | Square root of 2; approximately 1.414. |

| Function | Summary |
| -- | -- |
| abs(number) number | Returns the absolute value of x. |
| acos(number) number | Returns the arccosine of x. |
| acosh(number) number | Returns the hyperbolic arccosine of x. |
| asin(number) number | Returns the arcsine of x. |
| asinh(number) number | Returns the hyperbolic arcsine of a number. |
| atan(number) number | Returns the arctangent of x. |
| atan2(number, number) number | Returns the arctangent of the quotient of its arguments. |
| atanh(number) number | Returns the hyperbolic arctangent of x. |
| cbrt(number) number | Returns the cube root of x. |
| ceil(number) number | Returns the smallest integer greater than or equal to x. |
| clz32(number) number | Returns the number of leading zero bits of the 32-bit integer x. |
| cos(number) number | Returns the cosine of x. |
| cosh(number) number | Returns the hyperbolic cosine of x. |
| exp(number) number | Returns e^x, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm). |
| expm1(number) number | Returns subtracting 1 from exp(x). |
| floor(number) number | Returns the largest integer less than or equal to x. |
| hypot(number, number) number | Returns the square root of the sum of squares of its arguments. |
| isNaN(number) bool | Returns whether x is not a number. |
| ln(number) number | Returns the natural logarithm (㏒e; also, ㏑) of x. |
| log(number, number) number | Returns the logarithm of y with base x. |
| log10(number) number | Returns the base-10 logarithm of x. |
| log1p(number) number | Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x. |
| log2(number) number | Returns the base-2 logarithm of x. |
| max(number, number) number | Returns the largest of two numbers. |
| min(number, number) number | Returns the smallest of two numbers. |
| mul32(number, number) number | Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed. |
| pow(number, number) number | Returns base x to the exponent power y (that is, x^y). |
| random() number | Returns a pseudo-random number between 0 and 1. |
| round(number) number | Returns the value of the number x rounded to the nearest integer. |
| sign(number) number | Returns the sign of the x, indicating whether x is positive, negative, or zero. |
| sin(number) number | Returns the sine of x. |
| sinh(number) number | Returns the hyperbolic sine of x. |
| sqrt(number) number | Returns the positive square root of x. |
| tan(number) number | Returns the tangent of x. |
| tanh(number) number | Returns the hyperbolic tangent of x. |
| trunc(number) number | Returns the integer portion of x, removing any fractional digits. |

[To Top.](#table-of-contents)

### Os Module.
Cyber's os module contains system level functions. It's still undecided as to how much should be included here so it's incomplete. You can still access os and libc functions yourself using Cyber's FFI or embedding API.

Sample usage:
```text
import os 'os'

map = os.getEnvAll()
for map each k, v:
    print '{k} -> {v}'
```
| Variable | Type | Summary |
| -- | -- | -- |
| cpu | string | The current cpu arch's tag name. |
| endian | #little, #big | The current arch's endianness. |
| stdin | File | Standard input file descriptor. |
| system | string | The current operating system's tag name. |
| vecBitSize | number | Default SIMD vector bit size. |

| Function | Summary |
| -- | -- |
| `args() list<rawstring>` | Returns the command line arguments as a list of `rawstring`s. |
| `createDir(path string) true \| error` | Creates the directory at `path`. Returns `true` if successful. | 
| `createFile(path string, truncate bool) File \| error` | Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated. |
| `cwd() string` | Returns the current working directory. |
| `exePath() string` | Returns the current executable's path. |
| `getEnv(string) string` | Returns an environment value by key. |
| `getEnvAll() map` | Returns all environment entries as a map. |
| `milliTime() number` | Return the calendar timestamp, in milliseconds, relative to UTC 1970-01-01. |
| `openFile(path string, mode (#read \| #write \| #readWrite)) File \| error` | Opens a file at the given `path` with the `#read`, `#write`, or `#readWrite` mode. |
| `openDir(path string) Dir \| error` | Invokes `openDir(path, false)`. |
| `openDir(path string, iterable bool) Dir \| error` | Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated. |
| `realPath(path string) string` | Returns the absolute path of the given path. |
| `removeDir(path string) true \| error` | Removes an empty directory at `path`. Returns `true` if successful. |
| `removeFile(path string) true \| error` | Removes the file at `path`. Returns `true` if successful. |
| `setEnv(key string, value string) none` | Sets an environment value by key. |
| `sleep(ms number) none` | Pauses the current thread for given milliseconds. |
| `unsetEnv(string) none` | Removes an environment value by key. |

#### object File
| Method | Summary |
| -- | -- |
| `read(n number) rawstring` | Reads at most `n` bytes as a `rawstring`. `n` must be at least 1. A result with length 0 indicates the end of file was reached. |
| `readToEnd() rawstring` | Reads to the end of the file and returns the content as a `rawstring`. |
| `seek(pos number) none` | Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid. |
| `seekFromCur(pos number) none` | Seeks the read/write position by `pos` bytes from the current position. |
| `seekFromEnd(pos number) none` | Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid. |
| `stat() map` | Returns info about the file as a map. |
| `streamLines() Iterable<rawstring>` | Equivalent to `streamLines(4096)`. |
| `streamLines(bufSize number) Iterable<rawstring>` | Returns an iterable that streams lines ending in `\n`, `\r`, `\r\n`, or the `EOF`. The lines returned include the new line character(s). A buffer size of `bufSize` bytes is allocated for reading. |
| `write(data (string \| rawstring)) number` | Writes a `string` or `rawstring` at the current file position. The number of bytes written is returned. |

#### object Dir
| Method | Summary |
| -- | -- |
| `iterator() Iterator<DirEntry> \| error` | Returns a new iterator over the directory entries. If this directory was not opened with the iterable flag, `error(#NotAllowed)` is returned instead. |
| `stat() map` | Returns info about the file as a map. |
| `walk() Iterator<DirWalkEntry> \| error` | Returns a new iterator over the directory recursive entries. If this directory was not opened with the iterable flag, `error(#NotAllowed)` is returned instead. |

#### map DirEntry
| Entry | Summary |
| -- | -- |
| `'name' -> rawstring` | The name of the file or directory. |
| `'type' -> #file \| #dir \| #unknown` | The type of the entry. |

#### map DirWalkEntry
| Entry | Summary |
| -- | -- |
| `'name' -> rawstring` | The name of the file or directory. |
| `'path' -> rawstring` | The path of the file or directory relative to the walker's root directory. |
| `'type' -> #file \| #dir \| #unknown` | The type of the entry. |

[To Top.](#table-of-contents)

### Test Module.
The `test` module contains utilities for testing.

Sample usage:
```text
import t 'test'

a = 123 + 321
try t.eq(a, 444)
```

| Function | Summary |
| -- | -- |
| `eq(a any, b any) true \| error` | Returns whether two values are equal. Returns `error(#AssertError)` if types do not match up. |
| `eqList(a any, b any) true \| error` | Returns true if two lists have the same size and the elements are equal as if `eq` was called on those corresponding elements. |
| `eqNear(a any, b any) true \| error` | Returns two numbers are near each other within epsilon 1e-5. |

[To Top.](#table-of-contents)

### FFI.
Cyber supports binding to an existing C ABI compatible library at runtime.
This allows you to call into dynamic libraries created in C or other languages.
Cyber uses `libtcc` to JIT compile the bindings so function calls are fast. `bindLib` is part of the core library and accepts the path to the library as a string and a list of [CFunc](#cfunc) or [CStruct](#cstruct) declarations.

```text
lib = bindLib('mylib.so', [
    CFunc{ sym: 'add', args: [#int, #int], ret: #int }
])
lib.add(123, 321)
```

If the path argument to `bindLib` is just a filename, the search steps for the library is specific to the operating system. Provide an absolute (eg. '/foo/mylib.so') or relative (eg. './mylib.so') path to load from a direct location instead. When the path argument is `none`, it loads the currently running executable as a library allowing you to bind exported functions from the Cyber CLI or your own embedded Cyber app/runtime.

When using `CFunc` or `CStruct` declarations, [tag literals](#tags) are used to map primitive types from Cyber to C and back.
The following binding types and conversions are supported:
| Binding Type | Cyber | C |
| ------------- | ------------- | ----- |
| #bool | bool | bool |
| #i8 | number | int8_t, signed char | 
| #u8 | number | uint8_t, unsigned char | 
| #i16 | number | int16_t, short | 
| #u16 | number | uint16_t, unsigned short | 
| #int, #i32 | number | int |
| #u32 | number | uint32_t, unsigned int |
| #i64 | number | int64_t, long long | 
| #u64 | number | uint64_t, unsigned long long | 
| #usize | number | size_t, uintptr_t | 
| #f32 | number | float |
| #f64 | number | double |
| #charPtrZ | rawstring | char* (null terminated) |
| #ptr | opaque | void* |

When `#charPtrZ` is declared as a binding to C, a Cyber string is duped to become null terminated and the C-function is responsible for calling `free` on the `char*` pointer.

By default `bindLib` returns an anonymous object with the binded C-functions as methods. This is convenient for using it like an object, but it's less optimal compared to binding as functions. If a config is passed into `bindLib` as the third argument, `genMap: true` makes `bindLib` return a map instead with the binded C-functions as Cyber functions.
The resulting object of `bindLib` holds a reference to an internal TCCState which owns the loaded JIT code.
Once the object is released by ARC, the TCCState is also released which removes the JIT code from memory.

[To Top.](#table-of-contents)

### CFunc.
The `CFunc` object lets you bind to a C-function. The `sym` field maps to the C-function's symbol name in the dynamic library. The `args` field declares the type mapping from Cyber to C-function's arguments. Finally, the `ret` field declares the type mapping from the C-function's return type to a Cyber type.

```text
lib = bindLib('mylib.so', [
    CFunc{ sym: 'add', args: [#int, #int], ret: #int }
])
lib.add(123, 321)
```
The example above maps to this C declaration in `mylib.so`:
```c
int add(int a, int b) {
    return a + b;
}
```

[To Top.](#table-of-contents)

### CStruct.
You can also bind object types to C-structs using the `CStruct` object. The `type` field accepts an object type symbol and `fields` indicates the mapping for each field in `type` to and from a C-struct.
After adding a `CStruct` declaration, you can use the object type symbol in CFunc `args` and `ret` and also other CStruct `fields`.
```text
object MyObject
    a number
    b string
    c bool

lib = bindLib('mylib.so', [
    CFunc{ sym: 'foo', args: [MyObject], ret: MyObject }
    CStruct{ fields: [#f64, #charPtrZ, #bool], type: MyObject }
])
res = lib.foo(MyObject{ a: 123, b: 'foo', c: true })
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

[To Top.](#table-of-contents)

### Error Handling.
In Cyber, errors are values and do not propogate up the call stack by default. Users are not forced to handle errors unless the error value is passed to a typed destination.

The `error` type is a primitive that contains either a tag or a tag literal. Tag literals can be used for convenience but the underlying id value can not be statically defined. Use your own tags if you want reliable id values. In a future version of Cyber, you'll be able to attach an optional payload value.
```text
func doThatThing():
    return error(#oops)

tagtype MyError:
    boom
    badArgument
    nameTooLong

err = error(MyError#boom)
```

The `try` expression wraps a value and guarantees a non error value is returned. If the value is an error, execution stops in the current block and the error is returned to the parent call site.
```text
func foo():
    try doSomething()
    return 123

-- If `doSomething()` returns an error, `res` contains the error and not `123`.
res = foo()
```

The `catch` expression returns a non error value or swallows the error and returns the `none` value. If a `then` clause follows, a default value is returned instead of the `none` value. An `as` clause lets you use the error inside the `then` expression.
```text
func foo():
    return error(#boom)

res = catch foo()
res = catch foo() then 123
res = catch foo() as err then 123

-- A catch block.
res = catch foo() then:
    break 123
```

### Unexpected Errors.
An unexpected error is an error that you don't plan on handling at runtime. In this scenario, you can prefer to fail-fast and `panic`.

Panics are similar to exceptions in other languages. Once the builtin `panic` is invoked, the current fiber stops execution and begins to unwind its call stack. Once the error is propagated to the root, the fiber ends and transitions to a panic state. If the main fiber ends this way, the VM begins to shutdown.
```text
func kaboom():
    panic(#danger)

kaboom()     -- Scripts ends and prints the stack trace.
```

While the error is propagated up the call stack, the fiber can regain control in a `recover` block. The recover block must be declared before the statement that triggers a panic.
```text
func kaboom():
    recover err:
        if err == #danger:
            print 'recovered from #danger'
    panic(#danger)

kaboom()    -- Prints recovered message and continues execution in this block.
```

[To Top.](#table-of-contents)

### Concurrency.
Cyber supports fibers as a concurrency mechanism. There are plans to support preemptive concurrency with async/await as well as multithreading.

[To Top.](#table-of-contents)

### Fibers.
Fibers in Cyber allow representing execution contexts as first-class values. They contain their own call stack and program counters. Fibers by themselves do not enable parallelism.

The `coinit` creates a new fiber from a function call syntax. Using `coyield` inside a function pauses the current fiber and execution is returned to the fiber that invoked `coresume`.
```text
count = 0
func foo():
    count += 1
    coyield
    count += 1
fiber = coinit foo()
print count          -- '0'
coresume fiber
print count          -- '1'
coresume fiber
print count          -- '2'
```
In Cyber, `coyield` can be used anywhere in a fiber's call stack.
```text
func foo():
    count += 1
    bar()
func bar():
    -- Nested coyield in call stack.
    coyield
    count += 1
fiber = coinit foo()
coresume fiber
```
`coresume` also returns the resulting value. In a future version of Cyber, you will be able to yield back results and pass values back when resuming.
```text
func foo():
    return 123
fiber = coinit foo()
print(coresume fiber)    -- '123'
```
Use `Fiber.status()` to get the current state of the fiber.
```text
func foo():
    coyield
    print 'done'
fiber = coinit foo()
print fiber.status()   -- '#paused'
coresume fiber
print fiber.status()   -- '#paused'
coresume fiber
print fiber.status()   -- '#done'
```
The main execution context is a fiber as well. Once the main fiber has finished, the VM is done and control is returned to the host.

Fibers are freed by ARC just like any other object. Once there are no references to the fiber, it begins to release it's child references by unwinding its call stack.

[To Top.](#table-of-contents)

### Metaprogramming

[To Top.](#table-of-contents)

### Runtime Eval

[To Top.](#table-of-contents)

### Compile-time
Compile-time evaluation is experimental. Currently it is used to call builtins for debugging the compiler.
```text
func foo(a):
    b = a + 1
    compt compilerDumpLocals()
```

### Generics

[To Top.](#table-of-contents)

### CYON
CYON or the Cyber object notation is similar to JSON. The format uses the same literal value semantics as Cyber.
```text
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

[To Top.](#table-of-contents)
