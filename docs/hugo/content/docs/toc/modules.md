---
title: "Modules"
weight: 5
---

# Modules.
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
var Root.foo = 123
print foo         -- Statement is ignored.

-- bar.cy
var Root.bar = 321
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
Static variable declarations from imports can have circular references. Read more about this in [Static Variables]({{<relref "/docs/toc/syntax#static-variables">}}).

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

var Root.bar = 234  -- Exported static variable.

type Thing object:  -- Exported type.
    var a float
```

## Module URI.
To get the absolute path of the current module, reference the compile-time variable `ModUri`.
This can be used with `os.dirName` to get the current module directory.
```cy
print #ModUri              -- Prints '/some/path/foo.cy'

import os
print os.dirName(#ModUri)  -- Prints '/some/path'
```

## Visibility.
The annotation `@hide` provides a hint to editors that a static symbol should not appear in the auto-complete. Despite this, the symbol is still reachable.

## Builtin Modules.
Builtin modules are the bare minimum that comes with Cyber. The [embeddable library]({{<relref "/docs/toc/embedding">}}) contains these modules and nothing more. They include:
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
{{<cy "func copy(val any) any">}}Copies a primitive value or creates a shallow copy of an object value.{{</cy>}}
{{<cy "func dump(val any) none">}}Prints the result of `toCyon` on a value.{{</cy>}}
{{<cy "func errorReport() string">}}{{</cy>}}
{{<cy "func getObjectRc(val any) int">}}Returns the current reference count of an object.{{</cy>}}
{{<cy "func is(a any, b any) bool">}}Returns whether two values refer to the same instance.{{</cy>}}
{{<cy "func isAlpha(val int) bool">}}Returns whether a rune is an alphabetic letter.{{</cy>}}
{{<cy "func isDigit(val int) bool">}}Returns whether a rune is a digit.{{</cy>}}
{{<cy "func must(val any) any">}}If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned.{{</cy>}}
{{<cy "func panic(err any) none">}}Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors]({{<relref "/docs/toc/errors#unexpected-errors">}}).{{</cy>}}
{{<cy "func parseCyber(src string) Map">}}Parses Cyber source string into structured map object.  Currently, only metadata about static declarations is made available but this will be extended to include an AST.{{</cy>}}
{{<cy "func parseCyon(src string) any">}}Parses a CYON string into a value.{{</cy>}}
{{<cy "func performGC() Map">}}Runs the garbage collector once to detect reference cycles and abandoned objects.  Returns the statistics of the run in a map value.{{</cy>}}
{{<cy "func print(str any) none">}}Prints a value. The host determines how it is printed.{{</cy>}}
{{<cy "func runestr(val int) string">}}Converts a rune to a string.{{</cy>}}
{{<cy "func toCyon(val any) string">}}Encodes a value to CYON string.{{</cy>}}
{{<cy "func typeof(val any) metatype">}}Returns the value's type as a `metatype` object.{{</cy>}}
{{<cy "func typesym(val any) symbol">}}Returns the value's type as one of the predefined symbols:  .float, .int, .bool, .object, .list, .map, .string, .array, .function, .fiber, .pointer, .symbol, .metatype, .none, .error{{</cy>}}
### `type bool`

{{<cy "func bool.'$call'(val any) bool">}}Converts a value to either `true` or `false`.{{</cy>}}
### `type error`

{{<cy "func sym() symbol">}}Return the underlying `symbol`.{{</cy>}}
{{<cy "func error.'$call'(val any) error">}}Create an error from an enum or symbol.{{</cy>}}
### `type int`

{{<cy "func $prefix~() int">}}{{</cy>}}
{{<cy "func $prefix-() int">}}{{</cy>}}
{{<cy "func $infix<(o any) bool">}}{{</cy>}}
{{<cy "func $infix<=(o any) bool">}}{{</cy>}}
{{<cy "func $infix>(o any) bool">}}{{</cy>}}
{{<cy "func $infix>=(o any) bool">}}{{</cy>}}
{{<cy "func $infix+(o any) int">}}{{</cy>}}
{{<cy "func $infix-(o any) int">}}{{</cy>}}
{{<cy "func $infix*(o any) int">}}{{</cy>}}
{{<cy "func $infix/(o any) int">}}{{</cy>}}
{{<cy "func $infix%(o any) int">}}{{</cy>}}
{{<cy "func $infix^(o any) int">}}{{</cy>}}
{{<cy "func $infix&(o any) int">}}{{</cy>}}
{{<cy "func $infix|(o any) int">}}{{</cy>}}
{{<cy "func $infix||(o any) int">}}{{</cy>}}
{{<cy "func $infix<<(o any) int">}}{{</cy>}}
{{<cy "func $infix>>(o any) int">}}{{</cy>}}
{{<cy "func fmt(kind symbol) string">}}Formats the integer using a kind specifier which can be binary `.b`,  octal `.o`, decimal `.d`, hexadecimal `.x`, ASCII `.c`.{{</cy>}}
{{<cy "func fmt(kind symbol, opts Map) string">}}`opts.pad` provides the ASCII rune that is used for padding with a string length of `opts.width`.{{</cy>}}
{{<cy "func int.'$call'(val any) int">}}Converts a value to an 48-bit integer.{{</cy>}}
### `type float`

{{<cy "func $prefix-() float">}}{{</cy>}}
{{<cy "func $infix<(o any) bool">}}{{</cy>}}
{{<cy "func $infix<=(o any) bool">}}{{</cy>}}
{{<cy "func $infix>(o any) bool">}}{{</cy>}}
{{<cy "func $infix>=(o any) bool">}}{{</cy>}}
{{<cy "func $infix+(o any) float">}}{{</cy>}}
{{<cy "func $infix-(o any) float">}}{{</cy>}}
{{<cy "func $infix*(o any) float">}}{{</cy>}}
{{<cy "func $infix/(o any) float">}}{{</cy>}}
{{<cy "func $infix%(o any) float">}}{{</cy>}}
{{<cy "func $infix^(o any) float">}}{{</cy>}}
{{<cy "func float.'$call'(val any) float">}}Converts the value to a `float`. Panics if type conversion fails.{{</cy>}}
### `type List`

{{<cy "func $index(idx any) any">}}{{</cy>}}
{{<cy "func $setIndex(idx any, val any) none">}}{{</cy>}}
{{<cy "func $slice(start any, end any) List">}}{{</cy>}}
{{<cy "func append(val any) none">}}Appends a value to the end of the list.{{</cy>}}
{{<cy "func concat(list List) none">}}Concats the elements of another list to the end of this list.{{</cy>}}
{{<cy "func insert(idx int, val any) none">}}Inserts a value at index `idx`.{{</cy>}}
{{<cy "func iterator() ListIterator">}}Returns a new iterator over the list elements.{{</cy>}}
{{<cy "func join(sep string) string">}}Returns a new string that joins the elements with `separator`.{{</cy>}}
{{<cy "func len() int">}}Returns the number of elements in the list.{{</cy>}}
{{<cy "func remove(idx int) none">}}Removes an element at index `idx`.{{</cy>}}
{{<cy "func resize(size int) none">}}Resizes the list to `len` elements. If the new size is bigger, `none` values  are appended to the list. If the new size is smaller, elements at the end of the list are removed.{{</cy>}}
{{<cy "func slice(start any, end any) List">}}{{</cy>}}
{{<cy "func List.fill(val any, n int) List">}}Creates a list with initial capacity of `n` and values set to `val`.  If the value is an object, it is shallow copied `n` times.{{</cy>}}
### `type ListIterator`

{{<cy "func next() any">}}{{</cy>}}
### `type tuple`

{{<cy "func $index(idx int) any">}}{{</cy>}}
### `type Map`

{{<cy "func $index(key any) any">}}{{</cy>}}
{{<cy "func $setIndex(key any, val any) none">}}{{</cy>}}
{{<cy "func remove(key any) none">}}Removes the element with the given key `key`.{{</cy>}}
{{<cy "func size() int">}}Returns the number of key-value pairs in the map.{{</cy>}}
{{<cy "func iterator() MapIterator">}}Returns a new iterator over the map elements.{{</cy>}}
### `type MapIterator`

{{<cy "func next() any">}}{{</cy>}}
### `type string`

{{<cy "func $infix+(o any) string">}}{{</cy>}}
{{<cy "func concat(o string) string">}}Returns a new string that concats this string and `str`.{{</cy>}}
{{<cy "func endsWith(suffix string) bool">}}Returns whether the string ends with `suffix`.{{</cy>}}
{{<cy "func find(needle string) any">}}Returns the first index of substring `needle` in the string or `none` if not found.{{</cy>}}
{{<cy "func findAnyRune(runes string) any">}}Returns the first index of any UTF-8 rune in `runes` or `none` if not found.{{</cy>}}
{{<cy "func findRune(rune int) int">}}Returns the first index of UTF-8 rune `needle` in the string or `none` if not found.{{</cy>}}
{{<cy "func insert(idx int, str string) string">}}Returns a new string with `str` inserted at index `idx`.{{</cy>}}
{{<cy "func isAscii() bool">}}Returns whether the string contains all ASCII runes.{{</cy>}}
{{<cy "func len() int">}}Returns the number of UTF-8 runes in the string.{{</cy>}}
{{<cy "func less(other string) bool">}}Returns whether this string is lexicographically before `other`.{{</cy>}}
{{<cy "func lower() string">}}Returns this string in lowercase.{{</cy>}}
{{<cy "func replace(needle string, replacement string) string">}}Returns a new string with all occurrences of `needle` replaced with `replacement`.{{</cy>}}
{{<cy "func repeat(n int) string">}}Returns a new string with this string repeated `n` times.{{</cy>}}
{{<cy "func runeAt(n int) int">}}Returns the UTF-8 rune at index `idx`.{{</cy>}}
{{<cy "func slice(start any, end any) string">}}Returns a slice into this string from `start` to `end` (exclusive) indexes. This is equivalent to using the slice index operator `[start..end]`.{{</cy>}}
{{<cy "func $slice(start any, end any) string">}}{{</cy>}}
{{<cy "func sliceAt(idx int) string">}}Returns the UTF-8 rune at index `idx` as a single rune string.{{</cy>}}
{{<cy "func $index(idx int) string">}}{{</cy>}}
{{<cy "func split(sep string) List">}}Returns a list of UTF-8 strings split at occurrences of `sep`.{{</cy>}}
{{<cy "func startsWith(prefix string) bool">}}Returns whether the string starts with `prefix`.{{</cy>}}
{{<cy "func trim(mode symbol, delims string) string">}}Returns the string with ends trimmed from runes in `delims`. `mode` can be .left, .right, or .ends.{{</cy>}}
{{<cy "func upper() string">}}Returns this string in uppercase.{{</cy>}}
{{<cy "func string.'$call'(val any) string">}}Converts a value to a string.{{</cy>}}
### `type array`

{{<cy "func $infix+(o any) array">}}{{</cy>}}
{{<cy "func concat(other array) array">}}Returns a new array that concats this array and `other`.{{</cy>}}
{{<cy "func decode() string">}}Calls decode(.utf8){{</cy>}}
{{<cy "func decode(encoding symbol) string">}}Decodes the array based on an `encoding`. Supported encodings: `.utf8`.  Returns the decoded string or throws `error.Decode`.{{</cy>}}
{{<cy "func endsWith(suffix array) bool">}}Returns whether the array ends with `suffix`.{{</cy>}}
{{<cy "func find(needle array) any">}}Returns the first index of `needle` in the array or `none` if not found.{{</cy>}}
{{<cy "func findAnyByte(bytes array) any">}}Returns the first index of any `bytes` in `arrays` or `none` if not found.{{</cy>}}
{{<cy "func findByte(byte int) any">}}Returns the first index of `byte` in the array or `none` if not found.{{</cy>}}
{{<cy "func fmt(kind symbol) string">}}Formats each byte in the array using a kind specifier which can be binary `.b`,  octal `.o`, decimal `.d`, hexadecimal `.x`, ASCII `.c`.  Each byte is zero padded.{{</cy>}}
{{<cy "func getByte(idx int) int">}}Returns the byte value (0-255) at the given index `idx`.{{</cy>}}
{{<cy "func getInt(idx int, endian symbol) int">}}Returns the int value of the 6 bytes starting from `idx` with the given endianness (.little or .big).{{</cy>}}
{{<cy "func getInt32(idx int, endian symbol) int">}}Returns the int value of the 4 bytes starting from `idx` with the given endianness (.little or .big).{{</cy>}}
{{<cy "func insert(idx int, arr array) array">}}Returns a new array with `arr` inserted at index `idx`.{{</cy>}}
{{<cy "func insertByte(idx int, byte int) array">}}Returns a new array with `byte` inserted at index `idx`.{{</cy>}}
{{<cy "func len() int">}}Returns the number of bytes in the array.{{</cy>}}
{{<cy "func repeat(n int) array">}}Returns a new array with this array repeated `n` times.{{</cy>}}
{{<cy "func replace(needle array, replacement array) array">}}Returns a new array with all occurrences of `needle` replaced with `replacement`.{{</cy>}}
{{<cy "func slice(start any, end any) array">}}Returns a slice into this array from `start` to `end` (exclusive) indexes. This is equivalent to using the slice index operator `[start..end]`.{{</cy>}}
{{<cy "func $slice(start any, end any) array">}}{{</cy>}}
{{<cy "func $index(idx int) int">}}{{</cy>}}
{{<cy "func split(sep array) List">}}Returns a list of arrays split at occurrences of `sep`.{{</cy>}}
{{<cy "func startsWith(prefix array) bool">}}Returns whether the array starts with `prefix`.{{</cy>}}
{{<cy "func trim(mode symbol, delims array) array">}}Returns the array with ends trimmed from runes in `delims`. `mode` can be .left, .right, or .ends.{{</cy>}}
{{<cy "func array.'$call'(val any) array">}}Converts a string to an byte `array`.{{</cy>}}
### `type arrayIterator`

### `type pointer`

{{<cy "func addr() int">}}Returns the memory address as an `int`. The value may be negative since it's  bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.{{</cy>}}
{{<cy "func asObject() any">}}Unsafe. Casts the pointer to a Cyber object. The object is retained before it's returned.{{</cy>}}
{{<cy "func fromCstr(offset int) array">}}Unsafe. Returns an `array` from a null terminated C string.{{</cy>}}
{{<cy "func get(offset int, ctype symbol) any">}}Unsafe. Dereferences the pointer at a byte offset and returns the C value converted to Cyber.{{</cy>}}
{{<cy "func set(offset int, val any) none">}}Unsafe. Converts the value to a compatible C value and writes it to a byte offset from this pointer.{{</cy>}}
{{<cy "func toArray(offset int, len int) array">}}Unsafe. Returns an `array` with a copy of the byte data starting from an offset to the specified length.{{</cy>}}
{{<cy "func pointer.'$call'(val any) pointer">}}Converts a `int` to a `pointer` value, or casts to a `pointer`. This is usually used with FFI.{{</cy>}}
### `type Fiber`

{{<cy "func status() symbol">}}{{</cy>}}
### `type metatype`

{{<cy "func id() int">}}{{</cy>}}
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
{{<cy "var e float">}}Euler's number and the base of natural logarithms; approximately 2.718.{{</cy>}}
{{<cy "var inf float">}}Infinity.{{</cy>}}
{{<cy "var log10e float">}}Base-10 logarithm of E; approximately 0.434.{{</cy>}}
{{<cy "var log2e float">}}Base-2 logarithm of E; approximately 1.443.{{</cy>}}
{{<cy "var ln10 float">}}Natural logarithm of 10; approximately 2.303.{{</cy>}}
{{<cy "var ln2 float">}}Natural logarithm of 2; approximately 0.693.{{</cy>}}
{{<cy "var maxSafeInt float">}}The maximum integer value that can be safely represented as a float. 2^53-1 or 9007199254740991.{{</cy>}}
{{<cy "var minSafeInt float">}}The minumum integer value that can be safely represented as a float. -(2^53-1) or -9007199254740991.{{</cy>}}
{{<cy "var nan float">}}Not a number. Note that nan == nan.  However, if a nan came from an arithmetic operation, the comparison is undefined.  Use `isNaN` instead.{{</cy>}}
{{<cy "var neginf float">}}Negative infinity.{{</cy>}}
{{<cy "var pi float">}}Ratio of a circle's circumference to its diameter; approximately 3.14159.{{</cy>}}
{{<cy "var sqrt1_2 float">}}Square root of ½; approximately 0.707.{{</cy>}}
{{<cy "var sqrt2 float">}}Square root of 2; approximately 1.414.{{</cy>}}
{{<cy "func abs(a float) float">}}Returns the absolute value of x.{{</cy>}}
{{<cy "func acos(a float) float">}}Returns the arccosine of x.{{</cy>}}
{{<cy "func acosh(a float) float">}}Returns the hyperbolic arccosine of x.{{</cy>}}
{{<cy "func asin(a float) float">}}Returns the arcsine of x.{{</cy>}}
{{<cy "func asinh(a float) float">}}Returns the hyperbolic arcsine of a number.{{</cy>}}
{{<cy "func atan(a float) float">}}Returns the arctangent of x.{{</cy>}}
{{<cy "func atan2(a float, b float) float">}}Returns the arctangent of the quotient of its arguments.{{</cy>}}
{{<cy "func atanh(a float) float">}}Returns the hyperbolic arctangent of x.{{</cy>}}
{{<cy "func cbrt(a float) float">}}Returns the cube root of x.{{</cy>}}
{{<cy "func ceil(a float) float">}}Returns the smallest integer greater than or equal to x.{{</cy>}}
{{<cy "func clz32(a float) float">}}Returns the number of leading zero bits of the 32-bit integer x.{{</cy>}}
{{<cy "func cos(a float) float">}}Returns the cosine of x.{{</cy>}}
{{<cy "func cosh(a float) float">}}Returns the hyperbolic cosine of x.{{</cy>}}
{{<cy "func exp(a float) float">}}Returns e^x, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).{{</cy>}}
{{<cy "func expm1(a float) float">}}Returns subtracting 1 from exp(x).{{</cy>}}
{{<cy "func floor(a float) float">}}Returns the largest integer less than or equal to x.{{</cy>}}
{{<cy "func frac(a float) float">}}Returns the fractional or decimal part of a float value.{{</cy>}}
{{<cy "func hypot(a float, b float) float">}}Returns the square root of the sum of squares of its arguments.{{</cy>}}
{{<cy "func isInt(a float) bool">}}Returns true if the float has no fractional part, otherwise false.{{</cy>}}
{{<cy "func isNaN(a float) bool">}}Returns whether x is not a number.{{</cy>}}
{{<cy "func ln(a float) float">}}Returns the natural logarithm (㏒e; also, ㏑) of x.{{</cy>}}
{{<cy "func log(a float, b float) float">}}Returns the logarithm of y with base x.{{</cy>}}
{{<cy "func log10(a float) float">}}Returns the base-10 logarithm of x.{{</cy>}}
{{<cy "func log1p(a float) float">}}Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.{{</cy>}}
{{<cy "func log2(a float) float">}}Returns the base-2 logarithm of x.{{</cy>}}
{{<cy "func max(a float, b float) float">}}Returns the largest of two numbers.{{</cy>}}
{{<cy "func min(a float, b float) float">}}Returns the smallest of two numbers.{{</cy>}}
{{<cy "func mul32(a float, b float) float">}}Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.{{</cy>}}
{{<cy "func pow(a float, b float) float">}}Returns base x to the exponent power y (that is, x^y).{{</cy>}}
{{<cy "func random() float">}}Returns a pseudo-random number between 0 and 1.{{</cy>}}
{{<cy "func round(a float) float">}}Returns the value of the number x rounded to the nearest integer.{{</cy>}}
{{<cy "func sign(a float) float">}}Returns the sign of the x, indicating whether x is positive, negative, or zero.{{</cy>}}
{{<cy "func sin(a float) float">}}Returns the sine of x.{{</cy>}}
{{<cy "func sinh(a float) float">}}Returns the hyperbolic sine of x.{{</cy>}}
{{<cy "func sqrt(a float) float">}}Returns the positive square root of x.{{</cy>}}
{{<cy "func tan(a float) float">}}Returns the tangent of x.{{</cy>}}
{{<cy "func tanh(a float) float">}}Returns the hyperbolic tangent of x.{{</cy>}}
{{<cy "func trunc(a float) float">}}Returns the integer portion of x, removing any fractional digits.{{</cy>}}
<!-- math.end -->

## Std Modules.
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
    print '$(k) -> $(v)'
```

<!-- os.start -->
{{<cy "var cpu string">}}The current cpu arch's tag name.{{</cy>}}
{{<cy "var endian symbol">}}The current arch's endianness: .little, .big{{</cy>}}
{{<cy "var stderr File">}}Standard error file descriptor.{{</cy>}}
{{<cy "var stdin File">}}Standard input file descriptor.{{</cy>}}
{{<cy "var stdout File">}}Standard output file descriptor.{{</cy>}}
{{<cy "var system string">}}The current operating system's tag name.{{</cy>}}
{{<cy "var vecBitSize int">}}Default SIMD vector bit size.{{</cy>}}
{{<cy "func access(path string, mode symbol) none">}}Attempts to access a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.  Throws an error if unsuccessful.{{</cy>}}
{{<cy "func args() List">}}Returns the command line arguments in a `List`.  Each argument is converted to a `string`.{{</cy>}}
{{<cy "func cacheUrl(url string) string">}}Returns the path of a locally cached file of `url`.  If no such file exists locally, it's fetched from `url`.{{</cy>}}
{{<cy "func copyFile(srcPath string, dstPath string) none">}}Copies a file to a destination path.{{</cy>}}
{{<cy "func createDir(path string) none">}}Creates the directory at `path`. Returns `true` if successful.{{</cy>}}
{{<cy "func createFile(path string, truncate bool) File">}}Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated.{{</cy>}}
{{<cy "func cstr(s any) pointer">}}Returns a null terminated C string.{{</cy>}}
{{<cy "func cwd() string">}}Returns the current working directory.{{</cy>}}
{{<cy "func dirName(path string) string">}}Returns the given path with its last component removed.{{</cy>}}
{{<cy "func execCmd(args List) Map">}}Runs a shell command and returns the stdout/stderr.{{</cy>}}
{{<cy "func exePath() string">}}Returns the current executable's path.{{</cy>}}
{{<cy "func exit(status int) none">}}Exits the program with a status code.{{</cy>}}
{{<cy "func fetchUrl(url string) array">}}Fetches the contents at `url` using the HTTP GET request method.{{</cy>}}
{{<cy "func free(ptr pointer) none">}}Frees the memory located at `ptr`.{{</cy>}}
{{<cy "func getEnv(key string) string">}}Returns an environment variable by key.{{</cy>}}
{{<cy "func getEnvAll() Map">}}Returns all environment variables as a `Map`.{{</cy>}}
{{<cy "func malloc(size int) pointer">}}Allocates `size` bytes of memory and returns a pointer.{{</cy>}}
{{<cy "func milliTime() float">}}Return the calendar timestamp, in milliseconds, relative to UTC 1970-01-01.  For an high resolution timestamp, use `now()`.{{</cy>}}
{{<cy "func newFFI() FFI">}}Returns a new FFI context for declaring C mappings and binding a dynamic library.{{</cy>}}
{{<cy "func now() float">}}Returns the current time (in high resolution seconds) since an arbitrary point in time.{{</cy>}}
{{<cy "func openDir(path string) Dir">}}Invokes `openDir(path, false)`.{{</cy>}}
{{<cy "func openDir(path string, iterable bool) Dir">}}Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated.{{</cy>}}
{{<cy "func openFile(path string, mode symbol) File">}}Opens a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.{{</cy>}}
{{<cy "func parseArgs(options List) Map">}}Given expected `ArgOption`s, returns a map of the options and a `rest` entry which contains the non-option arguments.{{</cy>}}
{{<cy "func readAll() string">}}Reads stdin to the EOF as a UTF-8 string.  To return the bytes instead, use `stdin.readAll()`.{{</cy>}}
{{<cy "func readFile(path string) string">}}Reads the file contents from `path` as a UTF-8 string.  To return the bytes instead, use `File.readAll()`.{{</cy>}}
{{<cy "func readLine() string">}}Reads stdin until a new line as a `string`. This is intended to read user input from the command line.  For bulk reads from stdin, use `stdin`.{{</cy>}}
{{<cy "func realPath(path string) string">}}Returns the absolute path of the given path.{{</cy>}}
{{<cy "func removeDir(path string) none">}}Removes an empty directory at `path`. Returns `true` if successful.{{</cy>}}
{{<cy "func removeFile(path string) none">}}Removes the file at `path`. Returns `true` if successful.{{</cy>}}
{{<cy "func setEnv(key string, val string) none">}}Sets an environment variable by key.{{</cy>}}
{{<cy "func sleep(ms float) none">}}Pauses the current thread for given milliseconds.{{</cy>}}
{{<cy "func unsetEnv(key string) none">}}Removes an environment variable by key.{{</cy>}}
{{<cy "func writeFile(path string, contents any) none">}}Writes `contents` as a string or bytes to a file.{{</cy>}}
### `type File`

{{<cy "func close() none">}}Closes the file handle. File ops invoked afterwards will return `error.Closed`.{{</cy>}}
{{<cy "func iterator() any">}}{{</cy>}}
{{<cy "func next() any">}}{{</cy>}}
{{<cy "func read(n int) array">}}Reads at most `n` bytes as an `array`. `n` must be at least 1.  A result with length 0 indicates the end of file was reached.{{</cy>}}
{{<cy "func readAll() array">}}Reads to the end of the file and returns the content as an `array`.{{</cy>}}
{{<cy "func seek(n int) none">}}Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid.{{</cy>}}
{{<cy "func seekFromCur(n int) none">}}Seeks the read/write position by `pos` bytes from the current position.{{</cy>}}
{{<cy "func seekFromEnd(n int) none">}}Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid.{{</cy>}}
{{<cy "func stat() Map">}}Returns info about the file as a `Map`.{{</cy>}}
{{<cy "func streamLines() File">}}Equivalent to `streamLines(4096)`.{{</cy>}}
{{<cy "func streamLines(bufSize int) File">}}Returns an iterable that streams lines ending in `\n`, `\r`, `\r\n`, or the `EOF`.  The lines returned include the new line character(s).  A buffer size of `bufSize` bytes is allocated for reading.  If `\r` is found at the end of the read buffer, the line is returned instead of  waiting to see if the next read has a connecting `\n`.{{</cy>}}
{{<cy "func write(val any) int">}}Writes a `string` or `array` at the current file position.  The number of bytes written is returned.{{</cy>}}
### `type Dir`

{{<cy "func iterator() DirIterator">}}Returns a new iterator over the directory entries.  If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.{{</cy>}}
{{<cy "func stat() Map">}}Returns info about the file as a `Map`.{{</cy>}}
{{<cy "func walk() DirIterator">}}Returns a new iterator over the directory recursive entries.  If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.{{</cy>}}
### `type DirIterator`

{{<cy "func next() any">}}{{</cy>}}
### `type FFI`

{{<cy "func bindCallback(fn any, params List, ret symbol) ExternFunc">}}Creates an `ExternFunc` that contains a C function pointer with the given signature.  The extern function is a wrapper that calls the provided user function.  Once created, the extern function is retained and managed by the FFI context.{{</cy>}}
{{<cy "func bindLib(path any) any">}}Calls `bindLib(path, [:])`.{{</cy>}}
{{<cy "func bindLib(path any, config Map) any">}}Creates a handle to a dynamic library and functions declared from `cfunc`.  By default, an anonymous object is returned with the C-functions binded as the object's methods.  If `config` contains `genMap: true`, a `Map` is returned instead with C-functions  binded as function values.{{</cy>}}
{{<cy "func bindObjPtr(obj any) pointer">}}Returns a Cyber object's pointer. Operations on the pointer is unsafe,  but it can be useful when passing it to C as an opaque pointer.  The object is also retained and managed by the FFI context.{{</cy>}}
{{<cy "func cbind(mt metatype, fields List) none">}}Binds a Cyber type to a C struct.{{</cy>}}
{{<cy "func cfunc(name string, params List, ret any) none">}}Declares a C function which will get binded to the library handle created from `bindLib`.{{</cy>}}
{{<cy "func new(ctype symbol) pointer">}}Allocates memory for a C struct or primitive with the given C type specifier.  A `pointer` to the allocated memory is returned.  Eventually this will return a `cpointer` instead which will be more idiomatic to use.{{</cy>}}
{{<cy "func unbindObjPtr(obj any) none">}}Releases the object from the FFI context.  External code should no longer use the object's pointer since it's not guaranteed to exist  or point to the correct object.{{</cy>}}
### `type CArray`

### `type CDimArray`

<!-- os.end -->
### `map DirEntry`
| Entry | Summary |
| -- | -- |
| `'name' -> array` | The name of the file or directory. |
| `'type' -> #file \| #dir \| #unknown` | The type of the entry. |

### `map DirWalkEntry`
| Entry | Summary |
| -- | -- |
| `'name' -> array` | The name of the file or directory. |
| `'path' -> array` | The path of the file or directory relative to the walker's root directory. |
| `'type' -> #file \| #dir \| #unknown` | The type of the entry. |

### `map ArgOption`
| Entry | Summary |
| -- | -- |
| `'name' -> string` | The name of the option to match excluding the hyphen prefix. eg. `-path` |
| `'type' -> metatype(string \| float \| boolean)` | Parse as given value type. |
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
{{<cy "func eq(a any, b any) any">}}Returns whether two values are equal.  Panics with `error.AssertError` if types or values do not match up.{{</cy>}}
{{<cy "func eqList(a any, b any) any">}}Returns true if two lists have the same size and the elements are equal  as if `eq` was called on those corresponding elements.{{</cy>}}
{{<cy "func eqNear(a any, b any) any">}}Returns two numbers are near each other within epsilon 1e-5.{{</cy>}}
{{<cy "func fail() any">}}{{</cy>}}
<!-- test.end -->