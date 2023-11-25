--| Creates a list with initial capacity of `n` and values set to `val`.
--| If the value is an object, it is shallow copied `n` times.
@host func arrayFill(val any, n int) List

--| Copies a primitive value or creates a shallow copy of an object value.
@host func copy(val any) any

--| Prints the result of `toCyon` on a value.
@host func dump(val any) none

@host func errorReport() string

--| Returns the current reference count of an object.
@host func getObjectRc(val any) none

--| Returns whether two values refer to the same instance.
@host func is(a any, b any) bool

--| Returns whether a rune is an alphabetic letter.
@host func isAlpha(val int) bool
--| Returns whether a rune is a digit.
@host func isDigit(val int) bool

--| If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned.
@host func must(val any) any

--| Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors]({{<relref "/docs/toc/errors#unexpected-errors">}}).
@host func panic(err any) none

--| Parses Cyber source string into structured map object.
--| Currently, only metadata about static declarations is made available but this will be extended to include an AST.
@host func parseCyber(src string) Map

--| Parses a CYON string into a value.
@host func parseCyon(src string) any

--| Runs the garbage collector once to detect reference cycles and abandoned objects.
--| Returns the statistics of the run in a map value.
@host func performGC() Map

--| Prints a value. The host determines how it is printed.
@host func print(str any) none

--| Converts a rune to a string.
@host func runestr(val int) string

--| Encodes a value to CYON string.
@host func toCyon(val any) string

--| Returns the value's type as a `metatype` object.
@host func typeof(val any) metatype

--| Returns the value's type as one of the predefined symbols:
--| .float, .int, .bool, .object, .list, .map, .string, .rawstring, .function, .fiber, .pointer, .symbol, .metatype, .none, .error
@host func typesym(val any) symbol

@host
type bool object

--| Converts a value to either `true` or `false`.
@host func bool.'$call'(val any) bool

@host
type error object:
    --| Return the underlying `symbol`.
    @host func sym() any

--| Create an error from an enum or symbol.
@host func error.'$call'(val any) error

@host
type int object:
    @host func '$prefix~'() int
    @host func '$prefix-'() int
    @host func '$infix<'(o any) bool
    @host func '$infix<='(o any) bool
    @host func '$infix>'(o any) bool
    @host func '$infix>='(o any) bool
    @host func '$infix+'(o any) int
    @host func '$infix-'(o any) int
    @host func '$infix*'(o any) int
    @host func '$infix/'(o any) int
    @host func '$infix%'(o any) int
    @host func '$infix^'(o any) int
    @host func '$infix&'(o any) int
    @host func '$infix|'(o any) int
    @host func '$infix||'(o any) int
    @host func '$infix<<'(o any) int
    @host func '$infix>>'(o any) int

    --| Formats the integer using a kind specifier which can be binary `.b`,
    --| octal `.o`, decimal `.d`, hexadecimal `.x`, ASCII `.c`.
    @host func fmt(kind symbol) any

    --| `opts.pad` provides the ASCII rune that is used for padding with a string length of `opts.width`.
    @host func fmt(kind symbol, opts Map) any

--| Converts a value to an 48-bit integer.
@host func int.'$call'(val any) int

@host
type float object:
    @host func '$prefix-'() float
    @host func '$infix<'(o any) bool
    @host func '$infix<='(o any) bool
    @host func '$infix>'(o any) bool
    @host func '$infix>='(o any) bool
    @host func '$infix+'(o any) float
    @host func '$infix-'(o any) float
    @host func '$infix*'(o any) float
    @host func '$infix/'(o any) float
    @host func '$infix%'(o any) float
    @host func '$infix^'(o any) float

--| Converts the value to a `float`. Panics if type conversion fails.
@host func float.'$call'(val any) float

@host
type List object:
    @host func '$index'(idx any) any
    @host func '$setIndex'(idx any, val any) none

    --| Appends a value to the end of the list.
    @host func append(val any) none

    --| Concats the elements of another list to the end of this list.
    @host func concat(list List) none

    --| Inserts a value at index `idx`.
    @host func insert(idx int, val any) any

    --| Returns a new iterator over the list elements.
    @host func iterator() any

    --| Returns a new string that joins the elements with `separator`.
    @host func joinString(sep string) string

    --| Returns the number of elements in the list.
    @host func len() int

    --| Removes an element at index `idx`.
    @host func remove(idx int) any

    --| Resizes the list to `len` elements. If the new size is bigger, `none` values
    --| are appended to the list. If the new size is smaller, elements at the end of the list are removed.
    @host func resize(size int) any

    --| Sorts the list with the given `less` function.
    --| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
    func sort(lessFn any) none:
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

@host
type ListIterator object:
    @host func next() any

@host
type tuple object:
    @host func '$index'(idx int) any

@host
type Map object:
    @host func '$index'(key any) any
    @host func '$setIndex'(key any, val any) none

    --| Removes the element with the given key `key`.
    @host func remove(key any) none

    --| Returns the number of key-value pairs in the map.
    @host func size() int

    --| Returns a new iterator over the map elements.
    @host func iterator() any

@host
type MapIterator object:
    @host func next() any
  
@host
type string object:
    @host func '$infix+'(o any) any

    --| Returns a new string that concats this string and `str`.
    @host func concat(o string) any

    --| Returns whether the string ends with `suffix`.
    @host func endsWith(suffix string) bool

    --| Returns the first index of substring `needle` in the string or `none` if not found.
    @host func find(needle string) any

    --| Returns the first index of any UTF-8 rune in `runes` or `none` if not found.
    @host func findAnyRune(runes string) any

    --| Returns the first index of UTF-8 rune `needle` in the string or `none` if not found.
    @host func findRune(rune int) int

    --| Returns a new string with `str` inserted at index `idx`.
    @host func insert(idx int, str string) string

    --| Returns whether the string contains all ASCII runes.
    @host func isAscii() bool

    --| Returns the number of UTF-8 runes in the string.
    @host func len() int

    --| Returns whether this string is lexicographically before `other`.
    @host func less(other string) bool

    --| Returns this string in lowercase.
    @host func lower() string

    --| Returns a new string with all occurrences of `needle` replaced with `replacement`.
    @host func replace(needle string, replacement string) string

    --| Returns a new string with this string repeated `n` times.
    @host func repeat(n int) any

    --| Returns the UTF-8 rune at index `idx`.
    @host func runeAt(n int) any

    --| Returns a slice into this string from `start` to `end` (exclusive) indexes. This is equivalent to using the slice index operator `[start..end]`.
    @host func slice(start any, end any) any

    @host func '$slice'(start any, end any) any

    --| Returns the UTF-8 rune at index `idx` as a single rune string.
    @host func sliceAt(idx int) any

    @host func '$index'(idx int) any

    --| Returns a list of UTF-8 strings split at occurrences of `sep`.
    @host func split(sep string) List

    --| Returns whether the string starts with `prefix`.
    @host func startsWith(prefix string) bool

    --| Returns the string with ends trimmed from runes in `delims`. `mode` can be .left, .right, or .ends.
    @host func trim(mode symbol, delims string) any

    --| Returns this string in uppercase.
    @host func upper() string

--| Converts a value to a string.
@host func string.'$call'(val any) string

@host
type array object:
    @host func '$infix+'(o any) any

    --| Returns the byte value (0-255) at the given index `idx`.
    @host func byteAt(idx int) int

    --| Returns a new array that concats this array and `other`.
    @host func concat(other array) array

    --| Calls decode(.utf8)
    @host func decode() any

    --| Decodes the array based on an `encoding`. Supported encodings: `.utf8`.
    --| Returns the decoded string or throws `error.Decode`.
    @host func decode(encoding symbol) string

    --| Returns whether the array ends with `suffix`.
    @host func endsWith(suffix array) bool

    --| Returns the first index of `needle` in the array or `none` if not found.
    @host func find(needle array) any

    --| Returns the first index of any `bytes` in `arrays` or `none` if not found.
    @host func findAnyByte(bytes array) any

    --| Returns the first index of `byte` in the array or `none` if not found.
    @host func findByte(byte int) any

    --| Formats each byte in the array using a kind specifier which can be binary `.b`,
    --| octal `.o`, decimal `.d`, hexadecimal `.x`, ASCII `.c`.
    --| Each byte is zero padded.
    @host func fmt(kind symbol) any

    --| Returns a new array with `arr` inserted at index `idx`.
    @host func insert(idx int, arr array) any

    --| Returns a new array with `byte` inserted at index `idx`.
    @host func insertByte(idx int, byte int) any

    --| Returns the number of bytes in the array.
    @host func len() int

    --| Returns a new array with this array repeated `n` times.
    @host func repeat(n int) any

    --| Returns a new array with all occurrences of `needle` replaced with `replacement`.
    @host func replace(needle array, replacement array) array

    --| Returns a slice into this array from `start` to `end` (exclusive) indexes. This is equivalent to using the slice index operator `[start..end]`.
    @host func slice(start any, end any) any

    @host func '$slice'(start any, end any) any

    @host func '$index'(idx int) int

    --| Returns a list of arrays split at occurrences of `sep`.
    @host func split(sep array) List

    --| Returns whether the array starts with `prefix`.
    @host func startsWith(prefix array) bool

    --| Returns the array with ends trimmed from runes in `delims`. `mode` can be .left, .right, or .ends.
    @host func trim(mode symbol, delims array) any

--| Converts a string to an byte `array`.
@host func array.'$call'(val any) array

@host
type pointer object:
    --| Unsafe. Casts the pointer to a Cyber object. The object is retained before it's returned.
    @host func asObject() any

    --| Returns the memory address as an `int`. The value may be negative since it's
    --| bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.
    @host func value() int

    --| Converts the value to a compatible C value and writes it to a byte offset from this pointer.
    @host func writeAt(idx int, val any) none

--| Converts a `int` to a `pointer` value, or casts to a `pointer`. This is usually used with FFI.
@host func pointer.'$call'(val any) pointer

@host
type Fiber object:
    @host func status() symbol

@host
type metatype object:
    @host func id() int