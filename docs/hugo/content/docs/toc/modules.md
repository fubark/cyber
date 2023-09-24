---
title: "Modules"
weight: 5
---

# Modules.
Modules have their own namespace and contain accessible static symbols. By default, importing another Cyber script returns a module with its declared symbols.

## Importing.
Import declarations create a local alias to the module referenced by the import specifier. The Cyber CLI comes with some builtin modules like `math` and `test`. If the specifier does not refer to a builtin module, it looks for a Cyber script file relative to the current script's directory. An embedder can integrate their own module loader.
```cy
import t 'test'
t.eq(123, 123)

-- Imports are static declarations so they can be anywhere in the script.
import m 'math'
print m.cos(0)

-- Loading another Cyber script.
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
var foo: 123
print foo         -- Statement is ignored.

-- bar.cy
var bar: 321
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
```cy
import { cos, pi } 'math'
print cos(pi)
```

## Exporting.
All static declarations are exported when the script's module is loaded.
```cy
func foo():         -- Exported static function.
    print 123

var bar: 234        -- Exported static variable.

type Thing object:  -- Exported type.
    a float
```

The annotation `@hide` provides a hint to editors that the static symbol should not appear in the auto-complete. Despite this, the symbol is still reachable.
> _Planned Feature_

## Builtin Modules.
Builtin modules are the bare minimum that comes with Cyber. The [embeddable library]({{<relref "/docs/toc/embedding">}}) contains these modules and nothing more. They include:
- [builtins](#builtins): Cyber related functions and commonly used utilities.
- [math](#math): Math constants and functions.
> _Incomplete: The docs for builtin modules are not completely up-to-date. They will be auto generated in the future._

## builtins.
The `builtins` module contains functions related to Cyber and common utilities. It is automatically imported into each script's namespace. 

Sample usage:
```cy
-- `print` and `typeid` are available without imports.
print 'hello'
var id = typeid('my str')
print id
```

| Function | Summary |
| ------------- | ----- |
| `arrayFill(val any, n int) List` | Creates a list with initial capacity of `n` and values set to `val`. If the value is an object, it is shallow copied `n` times. | 
| `boolean(val any) boolean` | Converts a value to either `true` or `false`. | 
| `copy(val any) any` | Copies a primitive value or creates a shallow copy of an object value. | 
| `error(e (enum \| symbol)) error` | Create an error from an enum or symbol. | 
| `evalJS(val string) none` | Evals JS from the host environment. This is only available in a web WASM build of Cyber. | 
| `float(val any) float` | Casts or converts the value to a `float`. Panics if type conversion fails. | 
| `int(val any) int` | Converts a value to an 32-bit integer. | 
| `isAlpha(val int) boolean` | Returns whether a rune is an alphabetic letter. | 
| `isDigit(val int) boolean` | Returns whether a rune is a digit. | 
| `must(val any) any \| noreturn` | If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned. | 
| `panic(e symbol) noreturn` | Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors]({{<relref "/docs/toc/errors#unexpected-errors">}}). |
| `parseCyber(src any) map` | Parses Cyber source string into structured map object. Currently, only metadata about static declarations is made available but this will be extended to include an AST. | 
| `parseCyon(src any) any` | Parses a CYON string into a value. | 
| `performGC() map` | Runs the garbage collector once to detect reference cycles and abandoned objects. Returns the statistics of the run in a map value. | 
| `pointer(val any) pointer` | Converts a `int` to a `pointer` value, or casts to a `pointer`. This is usually used with FFI. | 
| `print(s string) none` | Prints a value. The host determines how it is printed. | 
| `rawstring(str string) rawstring` | Converts a string to a `rawstring`. | 
| `runestr(val int) string` | Converts a rune to a string. | 
| `string(val any) string` | Converts a value to a string. | 
| `toCyon(val any) string` | Encodes a value to CYON string. | 
| `typeof(any) metatype` | Returns the value's type as a `metatype` object. |
| `typesym(any) symbol` | Returns the value's type as one of the predefined symbols: #float, #int, #boolean, #object, #list, #map, #string, #rawstring, #function, #fiber, #pointer, #symbol, #metatype, #none, #error |

## math.
The math module contains commonly used math constants and functions.

Sample usage:
```cy
import m 'math'

var r = 10
print(m.pi * r^2)
```
| Variable | Type | Summary |
| ------------- | ------------- | ----- |
| e | float | Euler's number and the base of natural logarithms; approximately 2.718. |
| inf | float | Infinity. |
| log10e | float | Base-10 logarithm of E; approximately 0.434. |
| log2e | float | Base-2 logarithm of E; approximately 1.443. |
| ln10 | float | Natural logarithm of 10; approximately 2.303. |
| ln2 | float | Natural logarithm of 2; approximately 0.693. |
| nan | float | Not a number. Note that nan == nan, however, if a nan came from an arithmetic operation, the comparison is undefined (it may be true or false, so it is not reliable). |
| neginf | float | Negative infinity. |
| pi | float | Ratio of a circle's circumference to its diameter; approximately 3.14159. |
| sqrt1_2 | float | Square root of ½; approximately 0.707. |
| sqrt2 | float | Square root of 2; approximately 1.414. |

| Function | Summary |
| -- | -- |
| abs(float) float | Returns the absolute value of x. |
| acos(float) float | Returns the arccosine of x. |
| acosh(float) float | Returns the hyperbolic arccosine of x. |
| asin(float) float | Returns the arcsine of x. |
| asinh(float) float | Returns the hyperbolic arcsine of a number. |
| atan(float) float | Returns the arctangent of x. |
| atan2(float, float) float | Returns the arctangent of the quotient of its arguments. |
| atanh(float) float | Returns the hyperbolic arctangent of x. |
| cbrt(float) float | Returns the cube root of x. |
| ceil(float) float | Returns the smallest integer greater than or equal to x. |
| clz32(float) float | Returns the number of leading zero bits of the 32-bit integer x. |
| cos(float) float | Returns the cosine of x. |
| cosh(float) float | Returns the hyperbolic cosine of x. |
| exp(float) float | Returns e^x, where x is the argument, and e is Euler's number (2.718…, the base of the natural logarithm). |
| expm1(float) float | Returns subtracting 1 from exp(x). |
| floor(float) float | Returns the largest integer less than or equal to x. |
| hypot(float, float) float | Returns the square root of the sum of squares of its arguments. |
| isNaN(float) bool | Returns whether x is not a number. |
| ln(float) float | Returns the natural logarithm (㏒e; also, ㏑) of x. |
| log(float, float) float | Returns the logarithm of y with base x. |
| log10(float) float | Returns the base-10 logarithm of x. |
| log1p(float) float | Returns the natural logarithm (㏒e; also ㏑) of 1 + x for the number x. |
| log2(float) float | Returns the base-2 logarithm of x. |
| max(float, float) float | Returns the largest of two numbers. |
| min(float, float) float | Returns the smallest of two numbers. |
| mul32(float, float) float | Returns the result of the 32-bit integer multiplication of x and y. Integer overflow is allowed. |
| pow(float, float) float | Returns base x to the exponent power y (that is, x^y). |
| random() float | Returns a pseudo-random number between 0 and 1. |
| round(float) float | Returns the value of the number x rounded to the nearest integer. |
| sign(float) float | Returns the sign of the x, indicating whether x is positive, negative, or zero. |
| sin(float) float | Returns the sine of x. |
| sinh(float) float | Returns the hyperbolic sine of x. |
| sqrt(float) float | Returns the positive square root of x. |
| tan(float) float | Returns the tangent of x. |
| tanh(float) float | Returns the hyperbolic tangent of x. |
| trunc(float) float | Returns the integer portion of x, removing any fractional digits. |

## Std Modules.
Std modules come with Cyber's CLI. They include:
- [os](#os): System level functions.
- [test](#test): Utilities for testing.
> _Incomplete: The docs for std modules are not completely up-to-date. They will be auto generated in the future._

## os.
Cyber's os module contains system level functions. It's still undecided as to how much should be included here so it's incomplete. You can still access os and libc functions yourself using Cyber's FFI or embedding API.

Sample usage:
```cy
import os 'os'

var map = os.getEnvAll()
for map each k, v:
    print '{k} -> {v}'
```
| Variable | Type | Summary |
| -- | -- | -- |
| cpu | string | The current cpu arch's tag name. |
| endian | #little, #big | The current arch's endianness. |
| stderr | File | Standard error file descriptor. |
| stdin | File | Standard input file descriptor. |
| stdout | File | Standard output file descriptor. |
| system | string | The current operating system's tag name. |
| vecBitSize | int | Default SIMD vector bit size. |

| Function | Summary |
| -- | -- |
| `access(path any, mode (#read \| #write \| #readWrite)) true \| error` | Attempts to access a file at the given `path` with the `#read`, `#write`, or `#readWrite` mode. Return true or an error. |
| `args() List<string \| rawstring>` | Returns the command line arguments as a list. Each argument is validated and returned as a UTF-8 `string` or `rawstring` if the validation failed. |
| `bindLib(path any, decls [](CFunc\|CStruct)) Object \| Map` | Calls `bindLib(path, decls, {})`. | 
| `bindLib(path any, decls [](CFunc\|CStruct), config: BindLibConfig) Object \| Map` | Creates an FFI binding to a dynamic library and it's symbols. By default, an anonymous object is returned with the C-functions binded as the object's methods. If `config` contains `genMap: true`, a `Map` is returned instead with C-functions binded as function values. | 
| `cacheUrl(url string) string` | Returns the path of a locally cached file of `url`. If no such file exists locally, it's fetched from `url`. |
| `copyFile(srcPath any, dstPath any) none \| error` | Copies a file to a destination path. |
| `createDir(path any) true \| error` | Creates the directory at `path`. Returns `true` if successful. | 
| `createFile(path any, truncate boolean) File \| error` | Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated. |
| `cstr(any) pointer` | Returns a null terminated C string. |
| `cwd() string` | Returns the current working directory. |
| `dirName(path any) string \| none` | Returns the given path with its last component removed. |
| `execCmd(args []string) Map{ out, err, exited }` | Runs a shell command and returns the stdout/stderr. | 
| `exePath() string` | Returns the current executable's path. |
| `exit(status int) noreturn` | Exits the program with a status code. | 
| `fetchUrl(url string) rawstring` | Fetches the contents at `url` using the HTTP GET request method. | 
| `free(ptr pointer) none` | Frees the memory located at `ptr`. |
| `fromCstr(pointer) rawstring` | Returns a `rawstring` from a null terminated C string. |
| `getEnv(key any) string \| none` | Returns an environment value by key. |
| `getEnvAll() Map` | Returns all environment entries as a `Map`. |
| `getInput() rawstring` | Reads stdin until a new line is reached. This is intended to read user input from the command line. For bulk reads from stdin, use `os.stdin`. | 
| `malloc(size int) pointer` | Allocates `size` bytes of memory and returns a pointer. |
| `milliTime() float` | Return the calendar timestamp, in milliseconds, relative to UTC 1970-01-01. |
| `openDir(path any) Dir \| error` | Invokes `openDir(path, false)`. |
| `openDir(path any, iterable boolean) Dir \| error` | Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated. |
| `openFile(path any, mode (#read \| #write \| #readWrite)) File \| error` | Opens a file at the given `path` with the `#read`, `#write`, or `#readWrite` mode. |
| `parseArgs(options list[ArgOption]) map` | Given expected `ArgOption`s, returns a map of the options and a `rest` entry which contains the non-option arguments. |
| `readAll() rawstring` | Reads stdin to the EOF as a `rawstring`. | 
| `readFile(path string) rawstring` | Reads the file contents into a `rawstring` value. | 
| `realPath(path any) string \| error` | Returns the absolute path of the given path. |
| `removeDir(path any) true \| error` | Removes an empty directory at `path`. Returns `true` if successful. |
| `removeFile(path any) true \| error` | Removes the file at `path`. Returns `true` if successful. |
| `setEnv(key any, value any) none` | Sets an environment value by key. |
| `sleep(ms float) none` | Pauses the current thread for given milliseconds. |
| `unsetEnv(key any) none` | Removes an environment value by key. |
| `writeFile(path string, contents string) none` | Writes a string value to a file. | 

### `type File`
| Method | Summary |
| -- | -- |
| `close() none` | Closes the file handle. File ops invoked afterwards will return `error.Closed`. |
| `read(n float) rawstring` | Reads at most `n` bytes as a `rawstring`. `n` must be at least 1. A result with length 0 indicates the end of file was reached. |
| `readToEnd() rawstring` | Reads to the end of the file and returns the content as a `rawstring`. |
| `seek(pos float) none` | Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid. |
| `seekFromCur(pos float) none` | Seeks the read/write position by `pos` bytes from the current position. |
| `seekFromEnd(pos float) none` | Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid. |
| `stat() Map` | Returns info about the file as a `Map`. |
| `streamLines() Iterable<rawstring>` | Equivalent to `streamLines(4096)`. |
| `streamLines(bufSize float) Iterable<rawstring>` | Returns an iterable that streams lines ending in `\n`, `\r`, `\r\n`, or the `EOF`. The lines returned include the new line character(s). A buffer size of `bufSize` bytes is allocated for reading. If `\r` is found at the end of the read buffer, the line is returned instead of waiting to see if the next read has a connecting `\n`. |
| `write(data (string \| rawstring)) float` | Writes a `string` or `rawstring` at the current file position. The number of bytes written is returned. |

### `type Dir`
| Method | Summary |
| -- | -- |
| `iterator() Iterator<DirEntry> \| error` | Returns a new iterator over the directory entries. If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead. |
| `stat() Map` | Returns info about the file as a `Map`. |
| `walk() Iterator<DirWalkEntry> \| error` | Returns a new iterator over the directory recursive entries. If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead. |

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

| Function | Summary |
| -- | -- |
| `eq(a any, b any) true \| error` | Returns whether two values are equal. Returns `error.AssertError` if types do not match up. |
| `eqList(a any, b any) true \| error` | Returns true if two lists have the same size and the elements are equal as if `eq` was called on those corresponding elements. |
| `eqNear(a any, b any) true \| error` | Returns two numbers are near each other within epsilon 1e-5. |
