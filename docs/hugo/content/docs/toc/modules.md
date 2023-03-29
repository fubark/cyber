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
    a number
```

In a future version of Cyber, the annotation `@hide` would provide a hint to editors that the static symbol should not appear in the auto-complete. Despite this, the symbol would still be reachable.

## Builtin Modules.

Cyber currently contains the builtin modules:
- [core](#core-module): Cyber related functions and commonly used utilities.
- [math](#math-module): Math constants and functions.
- [os](#os-module): System level functions.
- [test](#test-module): Utilities for testing.

### Core Module.
The core module contains functions related to Cyber and common utilities. It is automatically imported into each script's namespace. 

Sample usage:
```cy
print 'hello'
contents = readFile 'foo.txt'
print contents
```

| Function | Summary |
| ------------- | ----- |
| `arrayFill(val any, n number) List` | Creates a list with initial capacity of `n` and values set to `val`. If the value is an object, it is shallow copied `n` times. | 
| `boolean(val any) boolean` | Converts a value to either `true` or `false`. | 
| `cacheUrl(url string) string` | Returns the path of a locally cached file of `url`. If no such file exists locally, it's fetched from `url`. |
| `copy(val any) any` | Copies a primitive value or creates a shallow copy of an object value. | 
| `execCmd(args []string) Map{ out, err, exited }` | Runs a shell command and returns the stdout/stderr. | 
| `exit(status number) noreturn` | Exits the program with a status code. | 
| `error(e (enum \| symbol)) error` | Create an error from an enum or symbol. | 
| `evalJS(val string) none` | Evals JS from the host environment. This is only available in a web WASM build of Cyber. | 
| `fetchUrl(url string) rawstring` | Fetches the contents at `url` using the HTTP GET request method. | 
| `getInput() rawstring` | Reads stdin until a new line is reached. This is intended to read user input from the command line. For bulk reads from stdin, use `os.stdin`. | 
| `int(val any) int` | Converts a value to an 32-bit integer. | 
| `must(val any) any \| noreturn` | If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned. | 
| `number(val any) number` | Casts or converts the value to a `number`. Panics if type conversion fails. | 
| `panic(e symbol) noreturn` | Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors]({{<relref "/docs/toc/errors#unexpected-errors">}}). |
| `parseCyon(cyon string) any` | Parses a CYON string into a value. | 
| `pointer(val any) pointer` | Converts a `number` to a `pointer` value, or casts to a `pointer`. This is usually used with FFI. | 
| `print(s string) none` | Prints a value as a string to stdout. The new line is also printed. | 
| `prints(s string) none` | Prints a value as a string to stdout. | 
| `rawstring(str string) rawstring` | Converts a string to a `rawstring`. | 
| `readAll() rawstring` | Reads stdin to the EOF as a `rawstring`. | 
| `readFile(path string) rawstring` | Reads the file contents into a `rawstring` value. | 
| `string(val any) string` | Converts a value to a string. | 
| `toCyon(val any) string` | Encodes a value to CYON string. | 
| `typeid(val any) number` | Returns the type id of the value. | 
| `typesym(any) symbol` | Returns the value's type as a symbol. |
| `writeFile(path string, contents string) none` | Writes a string value to a file. | 

### Math Module.
The math module contains commonly used math constants and functions.

Sample usage:
```cy
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

### Os Module.
Cyber's os module contains system level functions. It's still undecided as to how much should be included here so it's incomplete. You can still access os and libc functions yourself using Cyber's FFI or embedding API.

Sample usage:
```cy
import os 'os'

map = os.getEnvAll()
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
| vecBitSize | number | Default SIMD vector bit size. |

| Function | Summary |
| -- | -- |
| `access(path any, mode (#read \| #write \| #readWrite)) true \| error` | Attempts to access a file at the given `path` with the `#read`, `#write`, or `#readWrite` mode. Return true or an error. |
| `args() List<rawstring>` | Returns the command line arguments as a list of `rawstring`s. |
| `bindLib(path any, decls [](CFunc\|CStruct)) Object \| Map` | Calls `bindLib(path, decls, {})`. | 
| `bindLib(path any, decls [](CFunc\|CStruct), config: BindLibConfig) Object \| Map` | Creates an FFI binding to a dynamic library and it's symbols. By default, an anonymous object is returned with the C-functions binded as the object's methods. If `config` contains `genMap: true`, a `Map` is returned instead with C-functions binded as function values. | 
| `copyFile(srcPath any, dstPath any) none \| error` | Copies a file to a destination path. |
| `createDir(path any) true \| error` | Creates the directory at `path`. Returns `true` if successful. | 
| `createFile(path any, truncate boolean) File \| error` | Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated. |
| `cwd() string` | Returns the current working directory. |
| `dirName(path any) string \| none` | Returns the given path with its last component removed. |
| `exePath() string` | Returns the current executable's path. |
| `free(ptr pointer) none` | Frees the memory located at `ptr`. |
| `getEnv(key any) string \| none` | Returns an environment value by key. |
| `getEnvAll() Map` | Returns all environment entries as a `Map`. |
| `malloc(size number) pointer` | Allocates `size` bytes of memory and returns a pointer. |
| `milliTime() number` | Return the calendar timestamp, in milliseconds, relative to UTC 1970-01-01. |
| `openDir(path any) Dir \| error` | Invokes `openDir(path, false)`. |
| `openDir(path any, iterable boolean) Dir \| error` | Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated. |
| `openFile(path any, mode (#read \| #write \| #readWrite)) File \| error` | Opens a file at the given `path` with the `#read`, `#write`, or `#readWrite` mode. |
| `realPath(path any) string \| error` | Returns the absolute path of the given path. |
| `removeDir(path any) true \| error` | Removes an empty directory at `path`. Returns `true` if successful. |
| `removeFile(path any) true \| error` | Removes the file at `path`. Returns `true` if successful. |
| `setEnv(key any, value any) none` | Sets an environment value by key. |
| `sleep(ms number) none` | Pauses the current thread for given milliseconds. |
| `unsetEnv(key any) none` | Removes an environment value by key. |

#### `type File`
| Method | Summary |
| -- | -- |
| `close() none` | Closes the file handle. File ops invoked afterwards will return `error.Closed`. |
| `read(n number) rawstring` | Reads at most `n` bytes as a `rawstring`. `n` must be at least 1. A result with length 0 indicates the end of file was reached. |
| `readToEnd() rawstring` | Reads to the end of the file and returns the content as a `rawstring`. |
| `seek(pos number) none` | Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid. |
| `seekFromCur(pos number) none` | Seeks the read/write position by `pos` bytes from the current position. |
| `seekFromEnd(pos number) none` | Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid. |
| `stat() Map` | Returns info about the file as a `Map`. |
| `streamLines() Iterable<rawstring>` | Equivalent to `streamLines(4096)`. |
| `streamLines(bufSize number) Iterable<rawstring>` | Returns an iterable that streams lines ending in `\n`, `\r`, `\r\n`, or the `EOF`. The lines returned include the new line character(s). A buffer size of `bufSize` bytes is allocated for reading. If `\r` is found at the end of the read buffer, the line is returned instead of waiting to see if the next read has a connecting `\n`. |
| `write(data (string \| rawstring)) number` | Writes a `string` or `rawstring` at the current file position. The number of bytes written is returned. |

#### `type Dir`
| Method | Summary |
| -- | -- |
| `iterator() Iterator<DirEntry> \| error` | Returns a new iterator over the directory entries. If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead. |
| `stat() Map` | Returns info about the file as a `Map`. |
| `walk() Iterator<DirWalkEntry> \| error` | Returns a new iterator over the directory recursive entries. If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead. |

#### `map DirEntry`
| Entry | Summary |
| -- | -- |
| `'name' -> rawstring` | The name of the file or directory. |
| `'type' -> #file \| #dir \| #unknown` | The type of the entry. |

#### `map DirWalkEntry`
| Entry | Summary |
| -- | -- |
| `'name' -> rawstring` | The name of the file or directory. |
| `'path' -> rawstring` | The path of the file or directory relative to the walker's root directory. |
| `'type' -> #file \| #dir \| #unknown` | The type of the entry. |

### Test Module.
The `test` module contains utilities for testing.

Sample usage:
```cy
import t 'test'

a = 123 + 321
t.eq(a, 444)
```

| Function | Summary |
| -- | -- |
| `eq(a any, b any) true \| error` | Returns whether two values are equal. Returns `error.AssertError` if types do not match up. |
| `eqList(a any, b any) true \| error` | Returns true if two lists have the same size and the elements are equal as if `eq` was called on those corresponding elements. |
| `eqNear(a any, b any) true \| error` | Returns two numbers are near each other within epsilon 1e-5. |
