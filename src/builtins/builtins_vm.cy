--|
--| Functions.
--|

func bitcast(#D type, val #S) D:
    return bitcast_(D, typeid[D], val, typeid[S])

@host -func bitcast_(#D type, dst_t int, val #S, src_t int) D

--| Copies a primitive value or creates a shallow copy of an object value.
func copy(val #T) T:
    return copy(typeid[T], val)

@host -func copy(type_id int, val #T) T

--| Dumps a detailed description of a value.
@host func dump(val any) void

--| Prints a value to the error stream. The host determines how it is printed.
@host func eprint(str any) void

@host func errorReport() String

--| Returns the current reference count of an object.
@host func getObjectRc(val any) int

--| Returns whether two values refer to the same instance.
@host func is(a any, b any) bool

--| Returns whether a rune is an alphabetic letter.
@host func isAlpha(val int) bool

--| Returns whether a rune is a digit.
@host func isDigit(val int) bool

--| Returns whether a boxed value is the `none` case of a generic `Option(T)` type.
--| This is temporary and may be removed in a future release.
@host func isNone(val any) bool

--| If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned.
@host func must(val any) any

--| Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors](#unexpected-errors).
@host func panic(err any) dyn

--| Runs the garbage collector once to detect reference cycles and abandoned objects.
--| Returns the statistics of the run in a map value.
@host func performGC() Map

func ptrcast(#D type, val #S) *D:
    return ptrcast_(D, typeid[S], val)

@host -func ptrcast_(#D type, src_t int, val #S) *D

--| Prints a value. The host determines how it is printed.
@host func print(str any) void

--| Queues a callback function as an async task.
@host func queueTask(fn Func() void) void

@host func refcast(#T type, ptr *T) &T

--| Converts a rune to a string.
@host func runestr(val int) String

func sizeof(#T type) int:
    return sizeof_(typeid[T])

@host func sizeof_(type_id int) int

--| Returns the type of an expression. 
func typeof(t ExprType) type:
    return t.getType()

--|
--| Value templates.
--|

--| Returns the type ID of a type.
func typeid[T type] int:
    return (T).id()

--|
--| Types.
--|

@host type void _

@host type bool #bool_t

--| Converts any value to either `true` or `false`.
--| Integers and floats equal to 0 return false.
--| Empty strings return false.
--| Otherwise, returns true.
@host func bool.$call(val any) bool

@host type symbol #int64_t

@host
type error #int64_t:
    --| Return the underlying `symbol`.
    @host func sym(self) symbol

--| Create an error from symbol.
@host func error.$call(val any) error

@host
type byte #int8_t:
    @host func '$prefix~'(self) byte
    @host func '$infix<'(self, o byte) bool
    @host func '$infix<='(self, o byte) bool
    @host func '$infix>'(self, o byte) bool
    @host func '$infix>='(self, o byte) bool
    @host func '$infix+'(self, o byte) byte
    @host func '$infix-'(self, o byte) byte
    @host func '$infix*'(self, o byte) byte
    @host func '$infix/'(self, o byte) byte
    @host func '$infix%'(self, o byte) byte
    @host func '$infix^'(self, o byte) byte
    @host func '$infix&'(self, o byte) byte
    @host func '$infix|'(self, o byte) byte
    @host func '$infix||'(self, o byte) byte
    @host func '$infix<<'(self, o int) byte
    @host func '$infix>>'(self, o int) byte

    --| Formats the byte using a NumberFormat.
    @host func fmt(self, format NumberFormat) String

    --| `opts.pad` provides the ASCII rune that is used for padding with a string length of `config.width`.
    @host='int.fmt2'
    func fmt(self, format NumberFormat, config Table) String

    @host func $call(b byte) byte

@host
type int #int64_t:
    @host func '$prefix~'(self) int
    @host func '$prefix-'(self) int
    @host func '$infix<'(self, o int) bool
    @host func '$infix<='(self, o int) bool
    @host func '$infix>'(self, o int) bool
    @host func '$infix>='(self, o int) bool
    @host func '$infix+'(self, o int) int
    @host func '$infix-'(self, o int) int
    @host func '$infix*'(self, o int) int
    @host func '$infix/'(self, o int) int
    @host func '$infix%'(self, o int) int
    @host func '$infix^'(self, o int) int
    @host func '$infix&'(self, o int) int
    @host func '$infix|'(self, o int) int
    @host func '$infix||'(self, o int) int
    @host func '$infix<<'(self, o int) int
    @host func '$infix>>'(self, o int) int

    --| Formats the integer using a NumberFormat.
    @host func fmt(self, format NumberFormat) String

    --| `opts.pad` provides the ASCII rune that is used for padding with a string length of `config.width`.
    @host='int.fmt2'
    func fmt(self, format NumberFormat, config Table) String

type NumberFormat enum:
    --| ASCII
    case asc
    --| binary
    case bin
    --| decimal
    case dec
    --| hexadecimal
    case hex
    --| octal
    case oct

--| Converts a value to an 48-bit integer.
@host func int.$call(val any) int

@host
type float #float64_t:
    @host func '$prefix-'(self) float
    @host func '$infix<'(self, o float) bool
    @host func '$infix<='(self, o float) bool
    @host func '$infix>'(self, o float) bool
    @host func '$infix>='(self, o float) bool
    @host func '$infix+'(self, o float) float
    @host func '$infix-'(self, o float) float
    @host func '$infix*'(self, o float) float
    @host func '$infix/'(self, o float) float
    @host func '$infix%'(self, o float) float
    @host func '$infix^'(self, o float) float

--| Converts the value to a `float`. Panics if type conversion fails.
@host func float.$call(val any) float

@host type placeholder1 _
@host type placeholder2 _
@host type placeholder3 _
@host type taglit _

@host type dyn _

@host type any _

@host type type _:
    @host func id(self) int

--| Return the type of a value.
--| See `typeof` to obtain the type of an expression at compile-time.
@host func type.$call(val any) type

-- @host type ListValue[T type] struct:
--     @host func $index(self, idx int) &T

--     func $index(self, range Range) []T:
--         return self.indexRange(([]T).id(), range)

--     @host func indexRange(self, slice_t int, range Range) RefSlice(self, []T)

--     --| Appends a value to the end of the list.
--     @host func append(self, val T) void

--     --| Appends the elements of another list to the end of this list.
--     @host func appendList(self, o ListValue[T]) void

--     --| Appends the elements of another list to the end of this list.
--     @host func appendSlice(self, slice []T) void

--     --| Inserts a value at index `idx`.
--     @host func insert(self, idx int, val T) void

--     --| Returns a new iterator over the list elements.
--     func iterator(self) ListIterator[T]:
--         return self.iterator_((ListIterator[T]).id())

@host type List[T type] _:
    -- val ListValue[T]

    @host func $index(self, idx int) T

    @host='List.$indexRange'
    func $index(self, range Range) List[T]

    @host func $setIndex(self, idx int, val T) void

    --| Appends a value to the end of the list.
    @host func append(self, val T) void

    --| Appends the elements of another list to the end of this list.
    @host func appendAll(self, list List[T]) void

    --| Inserts a value at index `idx`.
    @host func insert(self, idx int, val T) void

    --| Returns a new iterator over the list elements.
    func iterator(self) ListIterator[T]:
        return self.iterator_(typeid[ListIterator[T]])

    @host -func iterator_(self, ret_type int) ListIterator[T]

    --| Returns a new string that joins the elements with `separator`.
    @host func join(self, sep String) String

    --| Returns the number of elements in the list.
    @host func len(self) int

    --| Removes an element at index `idx`.
    @host func remove(self, idx int) void

    --| Resizes the list to `len` elements. If the new size is bigger, `none` values
    --| are appended to the list. If the new size is smaller, elements at the end of the list are removed.
    func resize(self, size int) void:
        self.resize_(typeid[T], size)

    @host func resize_(self, elem_t int, size int) void

    --| Sorts the list with the given `less` function.
    --| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
    func sort(self, lessFn Func(T, T) bool) void:
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
func List.fill(val #T, n int) List[T]:
    return List.fill(typeid[List[T]], typeid[T], val, n)

@host -func List.fill(list_t int, val_t int, val #T, n int) List[T]

@host type ListIterator[T type] _:
    func next(self) ?T:
        return self.next_(typeid[Option[T]])

    @host func next_(self, ret_t int) ?T

@host
type Tuple _:
    @host func $index(self, idx int) any

@host type FuncSig #int64_t

-- Function pointer.
@host type funcptr_t[SIG FuncSig] _

-- Function union.
@host type funcunion_t[SIG FuncSig] _

-- Any function.
@host type Func _

-- Function symbol.
@host type funcsym_t[SIG FuncSig] _

@host
type Table:
    table_data Map

    @host func $initPair(self, key String, value any) void

    @host func $get(self, name String) dyn

    @host func $set(self, name String, value any)

    @host func $index(self, key any) dyn

    @host func $setIndex(self, key any, value any) void

@host
type Map _:
    @host func $initPair(self, key any, value any) void

    @host func $index(self, key any) dyn
    @host func $setIndex(self, key any, val any) void

    --| Returns whether there is a value mapped to `key`.
    @host func contains(self, key any) bool

    --| Returns value mapped to `key` or returns `none`.
    @host func get(self, key any) ?any

    --| Removes the element with the given key `key`.
    @host func remove(self, key any) bool

    --| Returns the number of key-value pairs in the map.
    @host func size(self) int

    --| Returns a new iterator over the map elements.
    @host func iterator(self) MapIterator

@host
type MapIterator _:
    @host func next(self) ?any
  
@host
type String _:
    --| Returns the rune at byte index `idx`. The replacement character (0xFFFD) is returned for an invalid UTF-8 rune.
    @host func $index(self, idx int) int

    --| Returns a slice into this string from a `Range` with `start` (inclusive) to `end` (exclusive) byte indexes.
    @host='String.$indexRange'
    func $index(self, range Range) String

    --| Returns a new string that concats this string and `str`.
    @host func '$infix+'(self, o any) String

    --| Returns a new string that concats this string and `str`.
    @host func concat(self, o String) String

    --| Returns the number of runes in the string.
    @host func count(self) int

    --| Calls decode(.utf8)
    -- @host func decode(self String) String

    --| Decodes the array based on an `encoding`. Supported encodings: `.utf8`.
    --| Returns the decoded string or throws `error.Decode`.
    -- @host='String.decode2'
    -- func decode(self String, encoding symbol) String

    --| Returns whether the string ends with `suffix`.
    @host func endsWith(self, suffix String) bool

    --| Returns the first byte index of substring `needle` in the string or `none` if not found. SIMD enabled.
    @host func find(self, needle String) ?int

    -- @host='String.findAnyByteSlice'
    -- func findAnyByte(self String, set []byte) ?int

    --| Returns the first index of any byte in `set` or `none` if not found.
    @host func findAnyByte(self, set List[byte]) ?int

    -- @host='String.findAnyRuneSlice'
    -- func findAnyRune(self String, runes []int) ?int

    --| Returns the first byte index of any rune in `runes` or `none` if not found. SIMD enabled.
    @host func findAnyRune(self, runes List[int]) ?int

    --| Returns the first index of `byte` in the array or `none` if not found.
    @host func findByte(self, b byte) ?int

    --| Returns the first byte index of a rune `needle` in the string or `none` if not found. SIMD enabled.
    @host func findRune(self, rune int) ?int

    --| Formats each byte in the string using a NumberFormat.
    --| Each byte is zero padded.
    @host func fmtBytes(self, format NumberFormat) String

    --| Returns the byte value (0-255) at the given index `idx`.
    @host func getByte(self, idx int) byte

    --| Returns the int value of the 6 bytes starting from `idx` with the given endianness (.little or .big).
    @host func getInt(self, idx int, endian symbol) int

    --| Returns the int value of the 4 bytes starting from `idx` with the given endianness (.little or .big).
    @host func getInt32(self, idx int, endian symbol) int

    --| Returns a new string with `str` inserted at byte index `idx`.
    @host func insert(self, idx int, str String) String

    --| Returns a new array with `byte` inserted at index `idx`.
    @host func insertByte(self, idx int, byte int) String

    --| Returns whether the string contains all ASCII runes.
    @host func isAscii(self) bool

    --| Returns the byte length of the string. See `count()` to obtain the number of runes.
    @host func len(self) int

    --| Returns whether this string is lexicographically before `other`.
    @host func less(self, other String) bool

    --| Returns this string in lowercase.
    @host func lower(self) String

    --| Returns a new string with all occurrences of `needle` replaced with `replacement`.
    @host func replace(self, needle String, replacement String) String

    --| Returns a new string with this string repeated `n` times.
    @host func repeat(self, n int) String

    --| Returns the starting byte index for the rune index `idx`.
    @host func seek(self, idx int) int

    --| Returns the UTF-8 rune starting at byte index `idx` as a string.
    @host func sliceAt(self, idx int) String

    --| Returns a list of UTF-8 strings split at occurrences of `sep`.
    @host func split(self, sep String) List[String]

    --| Returns whether the string starts with `prefix`.
    @host func startsWith(self, prefix String) bool

    --| Returns the string with ends trimmed from runes in `delims`. `mode` can be .left, .right, or .ends.
    @host func trim(self, mode symbol, delims String) String

    --| Returns this string in uppercase.
    @host func upper(self) String

--| Converts a value to a string.
@host func String.$call(val any) String

@host type array_t[N int, T type] _

type Array[N int, T type] array_t[N, T]:
    func $index(self, idx int) &T:
        return self.index(N, typeid[T], idx)

    @host -func index(self, n int, elem_t int, idx int) &T

    -- --| Returns a slice into this array from a `Range` with `start` (inclusive) to `end` (exclusive) indexes.
    -- func $index(self, range Range) []T:
    --     return self.indexRange(([]T).id(), range)

    -- @host -func indexRange(self, slice_t int, range Range) []T

    -- @host func '$infix+'(self, o Array[#M, T]) Array[N + M, T]

    -- --| Returns a new array that concats this array and `other`.
    -- @host func concat(self, other Array[#M, T]) Array[N + M, T]

    -- --| Returns a new array with `arr` inserted at index `idx`.
    -- @host func insert(self, idx int, arr Array) Array

    --| Returns a new iterator over the array.
    func iterator(self) RefSliceIterator[T]:
        var slice = &self
        return slice.iterator()

    --| Returns the number of elements in the array.
    func len(self) int:
        return N

    -- --| Returns a new array with this array repeated `n` times.
    -- @host func repeat(self, #M int) Array[N * M, T]

type pointer[T type] #int64_t:
    func $index(self, idx int) *T:
        return self.index(typeid[T], idx)

    @host -func index(self, elem_t int, idx int) *T

    func $index(self, range Range) [*]T:
        return self.indexRange(typeid[[*]T], range)

    @host -func indexRange(self, slice_t int, range Range) [*]T

    func $setIndex(self, idx int, val T) void:
        self.setIndex(typeid[T], idx, val)

    @host -func setIndex(self, elem_t int, idx int, val T) void

    --| When pointer runtime safety is enabled, this returns the raw pointer address as an `int64`. 
    --| Otherwise, the pointer itself is bitcasted to an `int64`.
    @host func addr(self) int

    --| Casts the pointer to a Cyber object. The object is retained before it's returned.
    @host func asObject(self) any

    --| Returns a `String` from a null terminated C string.
    @host func fromCstr(self, offset int) String

    --| Dereferences the pointer at a byte offset and returns the C value converted to Cyber.
    @host func get(self, offset int, ctype symbol) dyn

    --| Returns a `String` with a copy of the byte data starting from an offset to the specified length.
    @host func getString(self, offset int, len int) String

    --| Converts the value to a compatible C value and writes it to a byte offset from this pointer.
    @host func set(self, offset int, ctype symbol, val any) void

--| Converts an `int` to a `pointer` value.
@host func pointer.$call(#T type, addr int) *T

@host
type ExternFunc _:
    --| Returns the memory address as an `int`. The value may be negative since it's
    --| bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.
    @host func addr(self) int

    @host func ptr(self) *void

@host
type Fiber _:
    @host func status(self) symbol

@host
type Range struct:
    start int
    end   int

@host type TccState _

type Option[T type] enum:
    case none
    case some T 

@host type Future[T type] _

--| Returns a `Future[T]` that has a completed value.
func Future.complete(val #T) Future[T]:
    return Future.complete_(val, typeid[Future[T]])
    
@host -func Future.complete_(val #T, ret_type int) Future[T]

func Future.new(#T type) Future[T]:
    return Future.new_(T, typeid[Future[T]])

@host -func Future.new_(#T type, ret_type int) Future[T]

@host type FutureResolver[T type] _:
    @host func complete(self, val T) void
    @host func future(self) Future[T]

func FutureResolver.new(#T type) FutureResolver[T]:
    return FutureResolver.new_(T, typeid[Future[T]], typeid[FutureResolver[T]])

@host -func FutureResolver.new_(#T type, future_t int, ret_t int) FutureResolver[T]

type Ref[T type] #int64_t

type RefSlice[T type] struct:
    ptr *T
    n   int

    func $index(self, idx int) &T:
        return refcast(T, self.ptr[idx])

    func $index(self, range Range) []T:
        var ptr_slice = self.ptr[range.start..range.end]
        return .{ ptr=ptr_slice.ptr, n=ptr_slice.n }

    func $setIndex(self, idx int, val T) void:
        self.ptr[idx] = val

    -- --| Returns whether the array ends with `suffix`.
    -- @host func endsWith(self, suffix []T) bool

    -- --| Returns the first index of `needle` in the slice or `none` if not found.
    -- @host func find(self, needle []T) ?int

    -- --| Returns the first index of any `bytes` in `set` or `none` if not found.
    -- @host func findAny(self, set []T) ?int

    -- --| Returns the first index of `needle` in the slice or `none` if not found.
    -- @host func findScalar(self, needle T) ?int

    func iterator(self) RefSliceIterator[T]:
        return RefSliceIterator[T]{slice=self, next_idx=0}

    func len(self) int:
        return self.n

    -- --| Returns a list of arrays split at occurrences of `sep`.
    -- @host func split(self, sep []T) List[Array]

    -- --| Returns whether the array starts with `prefix`.
    -- @host func startsWith(self, prefix []T) bool

    -- --| Returns a slice with ends trimmed from `delims`. `mode` can be .left, .right, or .ends.
    -- @host func trim(self, mode symbol, delims []T) []T

type RefSliceIterator[T type]:
    slice    []T
    next_idx int

    func next(self) ?T:
        if self.next_idx >= self.slice.len():
            return none
        var res = self.slice[self.next_idx]
        self.next_idx += 1
        return res

type PtrSlice[T type] struct:
    ptr *T
    n   int

    func $index(self, idx int) *T:
        return self.ptr[idx]

    func $index(self, range Range) [*]T:
        return self.ptr[range.start..range.end]

    func $setIndex(self, idx int, val T) void:
        self.ptr[idx] = val

    --| Returns whether the array ends with `suffix`.
    func endsWith(self, suffix [*]T) bool:
        if suffix.len() > self.len():
            return false
        for self[self.len()-suffix.len()..] -> elem, i:
            if elem != suffix[i]:
                return false
        return true

    -- --| Returns the first index of `needle` in the slice or `none` if not found.
    -- @host func find(self, needle [*]T) ?int

    -- --| Returns the first index of any `bytes` in `set` or `none` if not found.
    -- @host func findAny(self, set [*]T) ?int

    --| Returns the first index of `needle` in the slice or `none` if not found.
    func findScalar(self, needle T) ?int:
        for self -> elem, i:
            if elem == needle: return i
        return none

    func iterator(self) PtrSliceIterator[T]:
        return PtrSliceIterator[T]{slice=self, next_idx=0}

    func len(self) int:
        return self.n

    -- TODO: Consider returning iterator instead.
    -- --| Returns a list of arrays split at occurrences of `sep`.
    -- @host func split(self, sep [*]T) List[Array]

    --| Returns whether the array starts with `prefix`.
    func startsWith(self, prefix [*]T) bool:
        if prefix.len() > self.len():
            return false
        for self[..prefix.len()] -> elem, i:
            if elem != prefix[i]:
                return false
        return true

    -- --| Returns a slice with ends trimmed from `delims`. `mode` can be .left, .right, or .ends.
    -- @host func trim(self, mode symbol, delims [*]T) [*]T

type PtrSliceIterator[T type]:
    slice    [*]T
    next_idx int

    func next(self) ?T:
        if self.next_idx >= self.slice.len():
            return none
        var res = self.slice[self.next_idx]
        self.next_idx += 1
        return res

type IMemory trait:
    func alloc(self, len int) [*]byte
    func free(self, buf [*]byte) void

@host
type Memory:
    iface IMemory

    func new(self, #T type) *T:
        var bytes = self.iface.alloc(sizeof(T))
        return ptrcast(T, bytes.ptr)

    func alloc(self, #T type, n int) [*]T:
        var bytes = self.iface.alloc(sizeof(T) * n)
        return ptrcast(T, bytes.ptr)[0..n]

    func free(self, ptr *#T):
        self.iface.free(ptrcast(byte, ptr)[0..sizeof(T)])

    func free(self, slice [*]#T):
        var bytes = ptrcast(byte, slice.ptr)[0..sizeof(T)*slice.len()]
        self.iface.free(bytes)

type DefaultMemory:
    with IMemory

    @host func alloc(self, len int) [*]byte
    @host func free(self, buf [*]byte) void

@host type ExprType type:
    @host func getType(self) type
