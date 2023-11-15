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

When the alias needs to be renamed, the import specifier comes after the alias name and must be a string literal.
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
> `func arrayFill(val any, n int) List`  
> Creates a list with initial capacity of `n` and values set to `val`.  If the value is an object, it is shallow copied `n` times.

> `func copy(val any) any`  
> Copies a primitive value or creates a shallow copy of an object value.

> `func dump(val any) string`  
> Prints the result of `toCyon` on a value.

> `func errorReport() string`  

> `func isAlpha(val int) boolean`  
> Returns whether a rune is an alphabetic letter.

> `func isDigit(val int) boolean`  
> Returns whether a rune is a digit.

> `func must(val any) any`  
> If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned.

> `func panic(err any) none`  
> Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors]({{<relref "/docs/toc/errors#unexpected-errors">}}).

> `func parseCyber(src any) Map`  
> Parses Cyber source string into structured map object.  Currently, only metadata about static declarations is made available but this will be extended to include an AST.

> `func parseCyon(src any) any`  
> Parses a CYON string into a value.

> `func performGC() Map`  
> Runs the garbage collector once to detect reference cycles and abandoned objects.  Returns the statistics of the run in a map value.

> `func print(str any) none`  
> Prints a value. The host determines how it is printed.

> `func runestr(val int) string`  
> Converts a rune to a string.

> `func toCyon(val any) string`  
> Encodes a value to CYON string.

> `func typeof(val any) metatype`  
> Returns the value's type as a `metatype` object.

> `func typesym(val any) symbol`  
> Returns the value's type as one of the predefined symbols:  .float, .int, .boolean, .object, .list, .map, .string, .rawstring, .function, .fiber, .pointer, .symbol, .metatype, .none, .error

### `type boolean`

> `func $call(val any) boolean`  
> Converts a value to either `true` or `false`.

### `type error`

> `func $call(val any) error`  
> Create an error from an enum or symbol.

> `func value(self any) any`  

### `type int`

> `func $call(val any) int`  
> Converts a value to an 48-bit integer.

> `func $prefix~(self any) int`  

> `func $prefix-(self any) int`  

> `func $infix<(self any, o any) boolean`  

> `func $infix<=(self any, o any) boolean`  

> `func $infix>(self any, o any) boolean`  

> `func $infix>=(self any, o any) boolean`  

> `func $infix+(self any, o any) int`  

> `func $infix-(self any, o any) int`  

> `func $infix*(self any, o any) int`  

> `func $infix/(self any, o any) int`  

> `func $infix%(self any, o any) int`  

> `func $infix^(self any, o any) int`  

> `func $infix&(self any, o any) int`  

> `func $infix|(self any, o any) int`  

> `func $infix||(self any, o any) int`  

> `func $infix<<(self any, o any) int`  

> `func $infix>>(self any, o any) int`  

### `type float`

> `func $call(val any) float`  
> Converts the value to a `float`. Panics if type conversion fails.

> `func $prefix-(self any) float`  

> `func $infix<(self any, o any) boolean`  

> `func $infix<=(self any, o any) boolean`  

> `func $infix>(self any, o any) boolean`  

> `func $infix>=(self any, o any) boolean`  

> `func $infix+(self any, o any) float`  

> `func $infix-(self any, o any) float`  

> `func $infix*(self any, o any) float`  

> `func $infix/(self any, o any) float`  

> `func $infix%(self any, o any) float`  

> `func $infix^(self any, o any) float`  

### `type List`

> `func $index(self any, idx any) any`  

> `func $setIndex(self any, idx any, val any) none`  

> `func add(self any, val any) none`  

> `func append(self any, val any) none`  
> Appends a value to the end of the list.

> `func concat(self any, list List) none`  
> Concats the elements of another list to the end of this list.

> `func insert(self any, idx int, val any) any`  
> Inserts a value at index `idx`.

> `func iterator(self any) any`  
> Returns a new iterator over the list elements.

> `func joinString(self any, sep any) string`  
> Returns a new string that joins the elements with `separator`.

> `func len(self any) int`  
> Returns the number of elements in the list.

> `func seqIterator(self any) any`  
> Returns a new sequence iterator over the list elements.

> `func remove(self any, idx int) any`  
> Removes an element at index `idx`.

> `func resize(self any, size int) any`  
> Resizes the list to `len` elements. If the new size is bigger, `none` values  are appended to the list. If the new size is smaller, elements at the end of the list are removed.

> `func sort(self any, lessFn any) any`  
> Sorts the list with the given `less` function.  If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.

### `type ListIterator`

> `func next(self any) any`  

> `func nextSeq(self any) any`  

### `type Map`

> `func $index(self any, key any) any`  

> `func $setIndex(self any, key any, val any) none`  

> `func remove(self any, key any) none`  
> Removes the element with the given key `key`.

> `func size(self any) int`  
> Returns the number of key-value pairs in the map.

> `func iterator(self any) any`  
> Returns a new iterator over the map elements.

> `func seqIterator(self any) any`  
> Returns a new sequence iterator over the map elements.

### `type MapIterator`

> `func next(self any) any`  

> `func nextSeq(self any) any`  

### `type pointer`

> `func $call(val any) pointer`  
> Converts a `int` to a `pointer` value, or casts to a `pointer`. This is usually used with FFI.

> `func value(self any) int`  
> Returns the memory address as an `int`. The value may be negative since it's  bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.

<!-- builtins.end -->

## math.
The math module contains commonly used math constants and functions.

Sample usage:
```cy
import math

var r = 10
print(math.pi * r^2)
```

<!-- math.start -->
> `var e float`  
> Euler's number and the base of natural logarithms; approximately 2.718.

> `var inf float`  
> Infinity.

> `var log10e float`  
> Base-10 logarithm of E; approximately 0.434.

> `var log2e float`  
> Base-2 logarithm of E; approximately 1.443.

> `var ln10 float`  
> Natural logarithm of 10; approximately 2.303.

> `var ln2 float`  
> Natural logarithm of 2; approximately 0.693.

> `var maxSafeInt float`  
> The maximum integer value that can be safely represented as a float. 2^53-1 or 9007199254740991.

> `var minSafeInt float`  
> The minumum integer value that can be safely represented as a float. -(2^53-1) or -9007199254740991.

> `var nan float`  
> Not a number. Note that nan == nan.  However, if a nan came from an arithmetic operation, the comparison is undefined.  Use `isNaN` instead.

> `var neginf float`  
> Negative infinity.

> `var pi float`  
> Ratio of a circle's circumference to its diameter; approximately 3.14159.

> `var sqrt1_2 float`  
> Square root of ½; approximately 0.707.

> `var sqrt2 float`  
> Square root of 2; approximately 1.414.

> `func abs(a float) float`  
> Returns the absolute value of x.

> `func acos(a float) float`  
> Returns the arccosine of x.

> `func acosh(a float) float`  
> Returns the hyperbolic arccosine of x.

> `func asin(a float) float`  
> Returns the arcsine of x.

> `func asinh(a float) float`  
> Returns the hyperbolic arcsine of a number.

> `func atan(a float) float`  
> Returns the arctangent of x.

> `func atan2(a float, b float) float`  
> Returns the arctangent of the quotient of its arguments.

> `func atanh(a float) float`  
> Returns the hyperbolic arctangent of x.

> `func cbrt(a float) float`  
> Returns the cube root of x.

> `func ceil(a float) float`  
> Returns the smallest integer greater than or equal to x.

> `func clz32(a float) float`  
> Returns the number of leading zero bits of the 32-bit integer x.

> `func cos(a float) float`  
> Returns the cosine of x.

> `func cosh(a float) float`  
> Returns the hyperbolic cosine of x.

> `func exp(a float) float`  
> Returns e^x, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm).

> `func expm1(a float) float`  
> Returns subtracting 1 from exp(x).

> `func floor(a float) float`  
> Returns the largest integer less than or equal to x.

> `func frac(a float) float`  
> Returns the fractional or decimal part of a float value.

> `func hypot(a float, b float) float`  
> Returns the square root of the sum of squares of its arguments.

> `func isInt(a float) boolean`  
> Returns true if the float has no fractional part, otherwise false.

> `func isNaN(a float) boolean`  
> Returns whether x is not a number.

> `func ln(a float) float`  
> Returns the natural logarithm (㏒e; also, ㏑) of x.

> `func log(a float, b float) float`  
> Returns the logarithm of y with base x.

> `func log10(a float) float`  
> Returns the base-10 logarithm of x.

> `func log1p(a float) float`  
> Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x.

> `func log2(a float) float`  
> Returns the base-2 logarithm of x.

> `func max(a float, b float) float`  
> Returns the largest of two numbers.

> `func min(a float, b float) float`  
> Returns the smallest of two numbers.

> `func mul32(a float, b float) float`  
> Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed.

> `func pow(a float, b float) float`  
> Returns base x to the exponent power y (that is, x^y).

> `func random() float`  
> Returns a pseudo-random number between 0 and 1.

> `func round(a float) float`  
> Returns the value of the number x rounded to the nearest integer.

> `func sign(a float) float`  
> Returns the sign of the x, indicating whether x is positive, negative, or zero.

> `func sin(a float) float`  
> Returns the sine of x.

> `func sinh(a float) float`  
> Returns the hyperbolic sine of x.

> `func sqrt(a float) float`  
> Returns the positive square root of x.

> `func tan(a float) float`  
> Returns the tangent of x.

> `func tanh(a float) float`  
> Returns the hyperbolic tangent of x.

> `func trunc(a float) float`  
> Returns the integer portion of x, removing any fractional digits.

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
for map each [k, v]:
    print '{k} -> {v}'
```

<!-- os.start -->
> `var cpu string`  
> The current cpu arch's tag name.

> `var endian symbol`  
> The current arch's endianness: .little, .big

> `var stderr any`  
> Standard error file descriptor.

> `var stdin any`  
> Standard input file descriptor.

> `var stdout any`  
> Standard output file descriptor.

> `var system string`  
> The current operating system's tag name.

> `var vecBitSize int`  
> Default SIMD vector bit size.

> `func access(path any, mode symbol) any`  
> Attempts to access a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.  Return true or an error.

> `func args() List`  
> Returns the command line arguments as a list.  Each argument is validated and returned as a UTF-8 `string` or `rawstring` if the validation failed.

> `func bindLib(path any, decls List) any`  
> Calls `bindLib(path, decls, {})`.

> `func bindLib(path any, decls List, config Map) any`  
> Creates an FFI binding to a dynamic library and it's symbols.  By default, an anonymous object is returned with the C-functions binded as the object's methods.  If `config` contains `genMap: true`, a `Map` is returned instead with C-functions  binded as function values.

> `func cacheUrl(url any) any`  
> Returns the path of a locally cached file of `url`.  If no such file exists locally, it's fetched from `url`.

> `func copyFile(srcPath any, dstPath any) any`  
> Copies a file to a destination path.

> `func createDir(path any) any`  
> Creates the directory at `path`. Returns `true` if successful.

> `func createFile(path any, truncate boolean) any`  
> Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated.

> `func cstr(s any) pointer`  
> Returns a null terminated C string.

> `func cwd() string`  
> Returns the current working directory.

> `func dirName(path any) any`  
> Returns the given path with its last component removed.

> `func execCmd(args List) any`  
> Runs a shell command and returns the stdout/stderr.

> `func exePath() string`  
> Returns the current executable's path.

> `func exit(status int) none`  
> Exits the program with a status code.

> `func fetchUrl(url any) any`  
> Fetches the contents at `url` using the HTTP GET request method.

> `func free(ptr pointer) none`  
> Frees the memory located at `ptr`.

> `func fromCstr(ptr pointer) rawstring`  
> Returns a `rawstring` from a null terminated C string.

> `func getEnv(key any) any`  
> Returns an environment value by key.

> `func getEnvAll() Map`  
> Returns all environment entries as a `Map`.

> `func getInput() any`  
> Reads stdin until a new line is reached. This is intended to read user input from the command line.  For bulk reads from stdin, use `os.stdin`.

> `func malloc(size int) pointer`  
> Allocates `size` bytes of memory and returns a pointer.

> `func milliTime() float`  
> Return the calendar timestamp, in milliseconds, relative to UTC 1970-01-01.

> `func openDir(path any) any`  
> Invokes `openDir(path, false)`.

> `func openDir(path any, iterable boolean) any`  
> Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated.

> `func openFile(path any, mode symbol) any`  
> Opens a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.

> `func parseArgs(options List) Map`  
> Given expected `ArgOption`s, returns a map of the options and a `rest` entry which contains the non-option arguments. |

> `func readAll() any`  
> Reads stdin to the EOF as a `rawstring`.

> `func readFile(path any) any`  
> Reads the file contents into a `rawstring` value.

> `func readLine() any`  

> `func realPath(path any) any`  
> Returns the absolute path of the given path.

> `func removeDir(path any) any`  
> Removes an empty directory at `path`. Returns `true` if successful.

> `func removeFile(path any) any`  
> Removes the file at `path`. Returns `true` if successful.

> `func setEnv(key any, val any) none`  
> Sets an environment value by key.

> `func sleep(ms float) none`  
> Pauses the current thread for given milliseconds.

> `func unsetEnv(key any) none`  
> Removes an environment value by key.

> `func writeFile(path any, contents any) any`  
> Writes a string value to a file.

### `type File`

> `func close(self any) none`  
> Closes the file handle. File ops invoked afterwards will return `error.Closed`.

> `func iterator(self any) any`  

> `func next(self any) any`  

> `func read(self any, n int) any`  
> Reads at most `n` bytes as a `rawstring`. `n` must be at least 1.  A result with length 0 indicates the end of file was reached.

> `func readToEnd(self any) any`  
> Reads to the end of the file and returns the content as a `rawstring`.

> `func seek(self any, n int) any`  
> Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid.

> `func seekFromCur(self any, n int) any`  
> Seeks the read/write position by `pos` bytes from the current position.

> `func seekFromEnd(self any, n int) any`  
> Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid.

> `func stat(self any) any`  
> Returns info about the file as a `Map`.

> `func streamLines(self any) any`  
> Equivalent to `streamLines(4096)`.

> `func streamLines(self any, bufSize int) any`  
> Returns an iterable that streams lines ending in `\n`, `\r`, `\r\n`, or the `EOF`.  The lines returned include the new line character(s).  A buffer size of `bufSize` bytes is allocated for reading.  If `\r` is found at the end of the read buffer, the line is returned instead of  waiting to see if the next read has a connecting `\n`.

> `func write(self any, val any) any`  
> Writes a `string` or `rawstring` at the current file position.  The number of bytes written is returned.

### `type Dir`

> `func iterator(self any) any`  
> Returns a new iterator over the directory entries.  If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.

> `func stat(self any) any`  
> Returns info about the file as a `Map`.

> `func walk(self any) any`  
> Returns a new iterator over the directory recursive entries.  If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.

### `type DirIterator`

> `func next(self any) any`  

<!-- os.end -->
### `map DirEntry`
| Entry | Summary |
| -- | -- |
| `'name' -> rawstring` | The name of the file or directory. |
| `'type' -> #file \| #dir \| #unknown` | The type of the entry. |

### `map DirWalkEntry`
| Entry | Summary |
| -- | -- |
| `'name' -> rawstring` | The name of the file or directory. |
| `'path' -> rawstring` | The path of the file or directory relative to the walker's root directory. |
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
> `func eq(a any, b any) any`  
> Returns whether two values are equal.  Panics with `error.AssertError` if types or values do not match up.

> `func eqList(a any, b any) any`  
> Returns true if two lists have the same size and the elements are equal  as if `eq` was called on those corresponding elements.

> `func eqNear(a any, b any) any`  
> Returns two numbers are near each other within epsilon 1e-5.

> `func fail() any`  

<!-- test.end -->