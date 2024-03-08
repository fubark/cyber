--| Copies a primitive value or creates a shallow copy of an object value.
#host func copy(val any) any

--| Prints the result of `toCyon` on a value.
#host func dump(val any) void

#host func errorReport() String

--| Returns the current reference count of an object.
#host func getObjectRc(val any) int

--| Returns whether two values refer to the same instance.
#host func is(a any, b any) bool

--| Returns whether a rune is an alphabetic letter.
#host func isAlpha(val int) bool

--| Returns whether a rune is a digit.
#host func isDigit(val int) bool

--| Returns whether a boxed value is the `none` case of a generic `Option(T)` type.
--| This is temporary and may be removed in a future release.
#host func isNone(val any) bool

--| If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned.
#host func must(val any) any

--| Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors](#unexpected-errors).
#host func panic(err any) dynamic

--| Parses Cyber source string into structured map object.
--| Currently, only metadata about static declarations is made available but this will be extended to include an AST.
#host func parseCyber(src String) Map

--| Parses a CYON string into a value.
#host func parseCyon(src String) any

--| Runs the garbage collector once to detect reference cycles and abandoned objects.
--| Returns the statistics of the run in a map value.
#host func performGC() Map

--| Prints a value. The host determines how it is printed.
#host func print(str any) void

--| Converts a rune to a string.
#host func runestr(val int) String

--| Encodes a value to CYON string.
#host func toCyon(val any) String

--| Returns the value's type as a `metatype` object.
#host func typeof(val any) metatype

--| Returns the value's type as one of the predefined symbols:
--| .float, .int, .bool, .object, .list, .map, .string, .array, .function, .fiber, .pointer, .symbol, .metatype, .none, .error
#host func typesym(val any) symbol

#host type void

#host type bool #bool_t

--| Converts a value to either `true` or `false`.
#host func bool.'$call'(val any) bool

#host type symbol #int64_t

#host
type error #int64_t:
    --| Return the underlying `symbol`.
    #host func sym() symbol

--| Create an error from an enum or symbol.
#host func error.'$call'(val any) error

#host
type int #int64_t:
    #host func '$prefix~'() int
    #host func '$prefix-'() int
    #host func '$infix<'(o any) bool
    #host func '$infix<='(o any) bool
    #host func '$infix>'(o any) bool
    #host func '$infix>='(o any) bool
    #host func '$infix+'(o any) int
    #host func '$infix-'(o any) int
    #host func '$infix*'(o any) int
    #host func '$infix/'(o any) int
    #host func '$infix%'(o any) int
    #host func '$infix^'(o any) int
    #host func '$infix&'(o any) int
    #host func '$infix|'(o any) int
    #host func '$infix||'(o any) int
    #host func '$infix<<'(o any) int
    #host func '$infix>>'(o any) int

    --| Formats the integer using a kind specifier which can be binary `.b`,
    --| octal `.o`, decimal `.d`, hexadecimal `.x`, ASCII `.c`.
    #host func fmt(kind symbol) String

    --| `opts.pad` provides the ASCII rune that is used for padding with a string length of `opts.width`.
    #host func fmt(kind symbol, opts Map) String

--| Converts a value to an 48-bit integer.
#host func int.'$call'(val any) int

#host
type float #float64_t:
    #host func '$prefix-'() float
    #host func '$infix<'(o any) bool
    #host func '$infix<='(o any) bool
    #host func '$infix>'(o any) bool
    #host func '$infix>='(o any) bool
    #host func '$infix+'(o any) float
    #host func '$infix-'(o any) float
    #host func '$infix*'(o any) float
    #host func '$infix/'(o any) float
    #host func '$infix%'(o any) float
    #host func '$infix^'(o any) float

--| Converts the value to a `float`. Panics if type conversion fails.
#host func float.'$call'(val any) float

#host type placeholder1
#host type placeholder2
#host type placeholder3

#host type dynamic

#host type any

#host type type

#host
type List:
    #host func '$index'(idx int) any
    #host func '$index'(range Range) List
    #host func '$setIndex'(idx int, val any) void

    --| Appends the elements of another list to the end of this list.
    #host func append(list List) void

    --| Appends a value to the end of the list.
    #host func append(val any) void

    --| Inserts a value at index `idx`.
    #host func insert(idx int, val any) void

    --| Returns a new iterator over the list elements.
    #host func iterator() ListIterator

    --| Returns a new string that joins the elements with `separator`.
    #host func join(sep String) String

    --| Returns the number of elements in the list.
    #host func len() int

    --| Removes an element at index `idx`.
    #host func remove(idx int) void

    --| Resizes the list to `len` elements. If the new size is bigger, `none` values
    --| are appended to the list. If the new size is smaller, elements at the end of the list are removed.
    #host func resize(size int) void

    #host func slice(start any, end any) List

    --| Sorts the list with the given `less` function.
    --| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
    func sort(lessFn any) void:
        -- Simple insertion sort, will be upgraded to pdqsort later on.
        for 1..self.len() -> i:
            var cur = self[i]
            var j = i-1
            while j >= 0:
                if lessFn(cur, self[j]):
                    self[j+1] = self[j]
                    j -= 1
                else: break
            self[j + 1] = cur

--| Creates a list with initial capacity of `n` and values set to `val`.
--| If the value is an object, it is shallow copied `n` times.
#host func List.fill(val any, n int) List

#host
type ListIterator:
    #host func next() ?any

#host
type Tuple:
    #host func '$index'(idx int) any

#host
type Map:
    #host func '$index'(key any) any
    #host func '$setIndex'(key any, val any) void

    --| Returns whether there is a value mapped to `key`.
    #host func contains(key any) bool

    --| Returns value mapped to `key` or returns `none`.
    #host func get(key any) ?any

    --| Removes the element with the given key `key`.
    #host func remove(key any) bool

    --| Returns the number of key-value pairs in the map.
    #host func size() int

    --| Returns a new iterator over the map elements.
    #host func iterator() MapIterator

#host
type MapIterator:
    #host func next() ?any
  
#host
type String:
    --| Returns a new string that concats this string and `str`.
    #host func '$infix+'(o any) String

    --| Returns a new string that concats this string and `str`.
    #host func concat(o String) String

    --| Returns the number of runes in the string.
    #host func count() int

    --| Returns whether the string ends with `suffix`.
    #host func endsWith(suffix String) bool

    --| Returns the first byte index of substring `needle` in the string or `none` if not found. SIMD enabled.
    #host func find(needle String) ?int

    --| Returns the first byte index of any rune in `runes` or `none` if not found. SIMD enabled.
    #host func findAnyRune(runes String) ?int

    --| Returns the first byte index of a rune `needle` in the string or `none` if not found. SIMD enabled.
    #host func findRune(rune int) ?int

    --| Returns a new string with `str` inserted at byte index `idx`.
    #host func insert(idx int, str String) String

    --| Returns whether the string contains all ASCII runes.
    #host func isAscii() bool

    --| Returns the byte length of the string. See `count()` to obtain the number of runes.
    #host func len() int

    --| Returns whether this string is lexicographically before `other`.
    #host func less(other String) bool

    --| Returns this string in lowercase.
    #host func lower() String

    --| Returns a new string with all occurrences of `needle` replaced with `replacement`.
    #host func replace(needle String, replacement String) String

    --| Returns a new string with this string repeated `n` times.
    #host func repeat(n int) String

    --| Returns the starting byte index for the rune index `idx`.
    #host func seek(idx int) int

    --| Returns the UTF-8 rune starting at byte index `idx` as a string.
    #host func sliceAt(idx int) String

    --| Returns the rune at byte index `idx`. The replacement character (0xFFFD) is returned for an invalid UTF-8 rune.
    #host func '$index'(idx int) String

    --| Returns a slice into this string from a `Range` with `start` (inclusive) to `end` (exclusive) byte indexes.
    #host func '$index'(range Range) String

    --| Returns a list of UTF-8 strings split at occurrences of `sep`.
    #host func split(sep String) List

    --| Returns whether the string starts with `prefix`.
    #host func startsWith(prefix String) bool

    --| Returns the string with ends trimmed from runes in `delims`. `mode` can be .left, .right, or .ends.
    #host func trim(mode symbol, delims String) String

    --| Returns this string in uppercase.
    #host func upper() String

--| Converts a value to a string.
#host func String.'$call'(val any) String

#host
type Array:
    #host func '$infix+'(o any) Array

    --| Returns a new array that concats this array and `other`.
    #host func concat(other Array) Array

    --| Calls decode(.utf8)
    #host func decode() String

    --| Decodes the array based on an `encoding`. Supported encodings: `.utf8`.
    --| Returns the decoded string or throws `error.Decode`.
    #host func decode(encoding symbol) String

    --| Returns whether the array ends with `suffix`.
    #host func endsWith(suffix Array) bool

    --| Returns the first index of `needle` in the array or `none` if not found.
    #host func find(needle Array) ?int

    --| Returns the first index of any `bytes` in `arrays` or `none` if not found.
    #host func findAnyByte(bytes Array) ?int

    --| Returns the first index of `byte` in the array or `none` if not found.
    #host func findByte(byte int) ?int

    --| Formats each byte in the array using a kind specifier which can be binary `.b`,
    --| octal `.o`, decimal `.d`, hexadecimal `.x`, ASCII `.c`.
    --| Each byte is zero padded.
    #host func fmt(kind symbol) String

    --| Returns the byte value (0-255) at the given index `idx`.
    #host func getByte(idx int) int

    --| Returns the int value of the 6 bytes starting from `idx` with the given endianness (.little or .big).
    #host func getInt(idx int, endian symbol) int

    --| Returns the int value of the 4 bytes starting from `idx` with the given endianness (.little or .big).
    #host func getInt32(idx int, endian symbol) int

    --| Returns a new array with `arr` inserted at index `idx`.
    #host func insert(idx int, arr Array) Array

    --| Returns a new array with `byte` inserted at index `idx`.
    #host func insertByte(idx int, byte int) Array

    --| Returns a new iterator over the array bytes.
    #host func iterator() ArrayIterator:
        return [ArrayIterator arr: self, nextIdx: 0]

    --| Returns the number of bytes in the array.
    #host func len() int

    --| Returns a new array with this array repeated `n` times.
    #host func repeat(n int) Array

    --| Returns a new array with all occurrences of `needle` replaced with `replacement`.
    #host func replace(needle Array, replacement Array) Array

    #host func '$index'(idx int) int

    --| Returns a slice into this array from a `Range` with `start` (inclusive) to `end` (exclusive) indexes.
    #host func '$index'(range Range) Array

    --| Returns a list of arrays split at occurrences of `sep`.
    #host func split(sep Array) List

    --| Returns whether the array starts with `prefix`.
    #host func startsWith(prefix Array) bool

    --| Returns the array with ends trimmed from runes in `delims`. `mode` can be .left, .right, or .ends.
    #host func trim(mode symbol, delims Array) Array

--| Converts a string to an byte `Array`.
#host func Array.'$call'(val any) Array

type ArrayIterator:
    arr     Array
    nextIdx int

    func next() ?any:
        if nextIdx >= self.arr.len():
            return none
        var res = self.arr[nextIdx]
        nextIdx += 1
        return res

#host
type pointer:
    --| Returns the memory address as an `int`. The value may be negative since it's
    --| bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.
    #host func addr() int

    --| Unsafe. Casts the pointer to a Cyber object. The object is retained before it's returned.
    #host func asObject() any

    --| Unsafe. Returns an `Array` from a null terminated C string.
    #host func fromCstr(offset int) Array

    --| Unsafe. Dereferences the pointer at a byte offset and returns the C value converted to Cyber.
    #host func get(offset int, ctype symbol) any

    --| Unsafe. Converts the value to a compatible C value and writes it to a byte offset from this pointer.
    #host func set(offset int, ctype symbol, val any) void

    --| Unsafe. Returns an `Array` with a copy of the byte data starting from an offset to the specified length.
    #host func toArray(offset int, len int) Array

--| Converts a `int` to a `pointer` value, or casts to a `pointer`. This is usually used with FFI.
#host func pointer.'$call'(val any) pointer

#host type Closure
#host type Lambda
#host type HostFunc

#host
type ExternFunc:
    --| Returns the memory address as an `int`. The value may be negative since it's
    --| bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.
    #host func addr() int

#host
type Fiber:
    #host func status() symbol

#host
type metatype:
    #host func id() int

#host type Range
#host type Box
#host type TccState

template(T type)
type Option enum:
    case none
    case some #T 
