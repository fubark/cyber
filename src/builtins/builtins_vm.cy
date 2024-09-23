--|
--| Functions.
--|

--| Returns all the current registered types when this function is invoked.
@host fn allTypes() List[type]

fn bitcast[D, S](D type, val S) D:
    return bitcast_(D, typeid[D], val, typeid[S])

@host -fn bitcast_[D, S](D type, dst_t int, val S, src_t int) D

--| Copies a primitive value or creates a shallow copy of an object value.
fn copy[T](val T) T:
    return copy_(typeid[T], val)

@host -fn copy_[T](type_id int, val T) T

--| Returns the tag of a choice.
--| TODO: This will be moved to `T.tag()`.
fn choicetag[T](choice T) T.Tag:
    return choicetag_(typeid[T.Tag], choice)

@host -fn choicetag_[T](tag_t int, choice T) T.Tag

--| Dumps a detailed description of a value.
@host fn dump(val any) void

--| Prints a value to the error stream. The host determines how it is printed.
@host fn eprint(str any) void

@host fn errorReport() string

--| Returns the current reference count of an object.
@host fn getObjectRc(val any) int

--| Returns whether two values refer to the same instance.
@host fn is(a any, b any) bool

--| Returns whether a rune is an alphabetic letter.
@host fn isAlpha(val int) bool

--| Returns whether a rune is a digit.
@host fn isDigit(val int) bool

--| Returns whether a boxed value is the `none` case of a generic `Option(T)` type.
--| This is temporary and may be removed in a future release.
@host fn isNone(val any) bool

--| If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned.
@host fn must(val any) any

--| Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors](#unexpected-errors).
@host fn panic(err any) dyn

--| Runs the cycle detector once to detect strong reference cycles and abandoned objects.
--| Returns the statistics of the run in a map value.
@host fn collectCycles() Map

fn ptrcast[D, S](D type, val S) *D:
    return ptrcast_(D, typeid[S], val)

@host -fn ptrcast_[D, S](D type, src_t int, val S) *D

--| Prints a value. The host determines how it is printed.
@host fn print(str any) void

--| Queues a callback function as an async task.
@host fn queueTask(func Fn() void) void

@host fn refcast[T](T type, ptr *T) ^T

--| Converts a rune to a string.
@host fn runestr(val int) string

fn sizeof[T](T type) int:
    return sizeof_(typeid[T])

@host fn sizeof_(type_id int) int

--| Returns the type of an expression. 
fn typeOf(t ExprType) type:
    return t.getType()

--| Returns info about a type.
@host fn typeInfo(T type) TypeInfo

--|
--| Value templates.
--|

--| Returns the type ID of a type.
def typeid[T type] int: 
    return (T).id()

--|
--| Types.
--|

@host type void _

@host type bool _

--| Converts any value to either `true` or `false`.
--| Integers and floats equal to 0 return false.
--| Empty strings return false.
--| Otherwise, returns true.
@host fn bool.$call(val any) bool

@host type symbol int

@host
type error int:
    --| Return the underlying `symbol`.
    @host fn sym(self) symbol

--| Create an error from symbol.
@host fn error.$call(val any) error

@host
type byte _:
    @host fn '$prefix~'(self) byte
    @host fn '$infix<'(self, o byte) bool
    @host fn '$infix<='(self, o byte) bool
    @host fn '$infix>'(self, o byte) bool
    @host fn '$infix>='(self, o byte) bool
    @host fn '$infix+'(self, o byte) byte
    @host fn '$infix-'(self, o byte) byte
    @host fn '$infix*'(self, o byte) byte
    @host fn '$infix/'(self, o byte) byte
    @host fn '$infix%'(self, o byte) byte
    @host fn '$infix^'(self, o byte) byte
    @host fn '$infix&'(self, o byte) byte
    @host fn '$infix|'(self, o byte) byte
    @host fn '$infix||'(self, o byte) byte
    @host fn '$infix<<'(self, o int) byte
    @host fn '$infix>>'(self, o int) byte

    --| Formats the byte using a NumberFormat.
    @host fn fmt(self, format NumberFormat) string

    --| `opts.pad` provides the ASCII rune that is used for padding with a string length of `config.width`.
    @host='int.fmt2'
    fn fmt(self, format NumberFormat, config ^Table) string

    @host fn $call(b byte) byte

@host
type int _:
    @host fn '$prefix~'(self) int
    @host fn '$prefix-'(self) int
    @host fn '$infix<'(self, o int) bool
    @host fn '$infix<='(self, o int) bool
    @host fn '$infix>'(self, o int) bool
    @host fn '$infix>='(self, o int) bool
    @host fn '$infix+'(self, o int) int
    @host fn '$infix-'(self, o int) int
    @host fn '$infix*'(self, o int) int
    @host fn '$infix/'(self, o int) int
    @host fn '$infix%'(self, o int) int
    @host fn '$infix^'(self, o int) int
    @host fn '$infix&'(self, o int) int
    @host fn '$infix|'(self, o int) int
    @host fn '$infix||'(self, o int) int
    @host fn '$infix<<'(self, o int) int
    @host fn '$infix>>'(self, o int) int

    fn fmt(self) string:
        return self.fmt(.dec)

    --| Formats the integer using a NumberFormat.
    @host fn fmt(self, format NumberFormat) string

    --| `opts.pad` provides the ASCII rune that is used for padding with a string length of `config.width`.
    @host='int.fmt2'
    fn fmt(self, format NumberFormat, config ^Table) string

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
@host fn int.$call(val any) int

@host
type float _:
    @host fn '$prefix-'(self) float
    @host fn '$infix<'(self, o float) bool
    @host fn '$infix<='(self, o float) bool
    @host fn '$infix>'(self, o float) bool
    @host fn '$infix>='(self, o float) bool
    @host fn '$infix+'(self, o float) float
    @host fn '$infix-'(self, o float) float
    @host fn '$infix*'(self, o float) float
    @host fn '$infix/'(self, o float) float
    @host fn '$infix%'(self, o float) float
    @host fn '$infix^'(self, o float) float

    @host fn fmt(self) string

--| Converts the value to a `float`. Panics if type conversion fails.
@host fn float.$call(val any) float

@host type placeholder1 _
@host type placeholder2 _
@host type placeholder3 _
@host type placeholder4 _
@host type placeholder5 _
@host type placeholder6 _
@host type taglit _

@host type dyn _

@host type any _

@host type type _:
    --| Returns a unique ID for this type.
    @host fn id(self) int

--| Return the type of a value.
--| See `typeof` to obtain the type of an expression at compile-time.
@host fn type.$call(val any) type

type TypeInfo enum:
    case int_t     IntInfo
    case array_t   ArrayInfo
    case bool_t    void
    case choice_t  ChoiceInfo
    case enum_t    EnumInfo
    case error_t   void
    case float_t   FloatInfo
    case func_t    FuncInfo
    case hostobj_t HostObjectInfo
    case opt_t     OptionInfo
    case ptr_t     PointerInfo
    case struct_t  StructInfo
    case trait_t   TraitInfo
    case type_t    void
    case void_t    void

type IntInfo struct:
    sign bool
    bits int

type FloatInfo struct:
    bits int

type HostObjectInfo struct:
    name   string
    -- getchildren
    -- finalizer

type EnumInfo struct:
    name  ?string
    cases List[EnumCase]

type EnumCase struct:
    name string

type ChoiceInfo struct:
    name  ?string
    cases List[ChoiceCase]

type ChoiceCase struct:
    name string
    type type

type FuncKind enum:
    case ptr
    case union
    case sym

type FuncInfo struct:
    kind   FuncKind
    params List[FuncParam]
    ret    type

type FuncParam struct:
    type type

type ArrayInfo struct:
    len  int
    elem type

type StructInfo struct:
    name   ?string
    fields List[StructField]
    -- funcs  List[funcsym_t]

type StructField struct:
    name   string
    type   type
    offset int

type TraitInfo struct:
    name string
    -- funcs List[funcsym_t]

type PointerInfo struct:
    child type
    ref   bool

type OptionInfo struct:
    elem type

@host type List[T type] _:

    @host fn $index(self, idx int) T

    fn $slice(self, start int, end int, has_end bool) List[T]:
        return self.slice(start, if (has_end) end else self.len())

    @host -fn slice(self, start int, end int) List[T]

    @host fn $setIndex(self, idx int, val T) void

    --| Appends a value to the end of the list.
    @host fn append(self, val T) void

    --| Appends the elements of another list to the end of this list.
    @host fn appendAll(self, list List[T]) void

    --| Inserts a value at index `idx`.
    @host fn insert(self, idx int, val T) void

    --| Returns a new iterator over the list elements.
    fn iterator(self) ListIterator[T]:
        return self.iterator_(typeid[ListIterator[T]])

    @host -fn iterator_(self, ret_type int) ListIterator[T]

    --| Returns a new string that joins the elements with `separator`.
    fn join(self, sep string) string:
        return self.join(typeid[T], sep)

    @host fn join(self, elem_t int, sep string) string

    --| Returns the number of elements in the list.
    @host fn len(self) int

    --| Removes an element at index `idx`.
    @host fn remove(self, idx int) void

    --| Resizes the list to `len` elements. If the new size is bigger, `none` values
    --| are appended to the list. If the new size is smaller, elements at the end of the list are removed.
    fn resize(self, size int) void:
        self.resize_(typeid[T], size)

    @host fn resize_(self, elem_t int, size int) void

    --| Sorts the list with the given `less` function.
    --| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
    fn sort(self, lessFn Fn(T, T) bool) void:
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
fn List.fill[T](val T, n int) List[T]:
    return List.fill_(typeid[List[T]], typeid[T], val, n)

@host -fn List.fill_[T](list_t int, val_t int, val T, n int) List[T]

@host type ListIterator[T type] _:
    fn next(self) ?T:
        return self.next_(typeid[Option[T]])

    @host fn next_(self, ret_t int) ?T

@host type FuncSig int

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

    @host fn $initPair(self, key string, value any) void

    @host fn $get(self, name string) dyn

    @host fn $set(self, name string, value any) void

    @host fn $index(self, key any) dyn

    @host fn $setIndex(self, key any, value any) void

@host
type Map _:
    @host fn $initPair(self, key any, value any) void

    @host fn $index(self, key any) dyn
    @host fn $setIndex(self, key any, val any) void

    --| Returns whether there is a value mapped to `key`.
    @host fn contains(self, key any) bool

    --| Returns value mapped to `key` or returns `none`.
    @host fn get(self, key any) ?dyn

    --| Removes the element with the given key `key`.
    @host fn remove(self, key any) bool

    --| Returns the number of key-value pairs in the map.
    @host fn size(self) int

    --| Returns a new iterator over the map elements.
    @host fn iterator(self) MapIterator

@host
type MapIterator _:
    @host fn next(self) ?MapEntry[dyn, dyn]

type MapEntry[K type, V type]:
    key   K
    value V

@host
type string _:
    --| Returns the rune at byte index `idx`. The replacement character (0xFFFD) is returned for an invalid UTF-8 rune.
    @host fn $index(self, idx int) int

    --| Returns a slice into this string from a `Range` with `start` (inclusive) to `end` (exclusive) byte indexes.
    @host fn $slice(self, start int, end int, has_end bool) string

    --| Returns a new string that concats this string and `str`.
    @host fn '$infix+'(self, str string) string

    --| Returns a new string that concats this string with a string representation of `v`.
    @host='string.$infix+_any' fn '$infix+'(self, v any) string

    --| Returns a new string that concats this string and `str`.
    @host fn concat(self, str string) string

    --| Returns a new string that concats this string with a string representation of `v`.
    @host='string.concat_any' fn concat(self, v any) string

    --| Returns the number of runes in the string.
    @host fn count(self) int

    --| Calls decode(.utf8)
    -- @host fn decode(self string) string

    --| Decodes the array based on an `encoding`. Supported encodings: `.utf8`.
    --| Returns the decoded string or throws `error.Decode`.
    -- @host='string.decode2'
    -- fn decode(self string, encoding symbol) string

    --| Returns whether the string ends with `suffix`.
    @host fn endsWith(self, suffix string) bool

    --| Returns the first byte index of substring `needle` in the string or `none` if not found. SIMD enabled.
    @host fn find(self, needle string) ?int

    -- @host='string.findAnyByteSlice'
    -- fn findAnyByte(self string, set []byte) ?int

    --| Returns the first index of any byte in `set` or `none` if not found.
    @host fn findAnyByte(self, set List[byte]) ?int

    -- @host='string.findAnyRuneSlice'
    -- fn findAnyRune(self string, runes []int) ?int

    --| Returns the first byte index of any rune in `runes` or `none` if not found. SIMD enabled.
    @host fn findAnyRune(self, runes List[int]) ?int

    --| Returns the first index of `byte` in the array or `none` if not found.
    @host fn findByte(self, b byte) ?int

    --| Returns the first byte index of a rune `needle` in the string or `none` if not found. SIMD enabled.
    @host fn findRune(self, rune int) ?int

    --| Replaces each placeholder `@` from the receiver string with the corresponding value in `args` converted to a string.
    @host fn fmt(self, args List[any]) string

    @host='string.fmt2' fn fmt(self, placeholder string, args List[any]) string

    --| Formats each byte in the string using a NumberFormat.
    --| Each byte is zero padded.
    @host fn fmtBytes(self, format NumberFormat) string

    --| Returns the byte value (0-255) at the given index `idx`.
    @host fn getByte(self, idx int) byte

    --| Returns the int value of the 6 bytes starting from `idx` with the given endianness (.little or .big).
    @host fn getInt(self, idx int, endian symbol) int

    --| Returns the int value of the 4 bytes starting from `idx` with the given endianness (.little or .big).
    @host fn getInt32(self, idx int, endian symbol) int

    --| Returns a new string with `str` inserted at byte index `idx`.
    @host fn insert(self, idx int, str string) string

    --| Returns a new array with `byte` inserted at index `idx`.
    @host fn insertByte(self, idx int, byte int) string

    --| Returns whether the string contains all ASCII runes.
    @host fn isAscii(self) bool

    fn iterator(self) StringIterator:
        return .{str=self, idx=0}

    --| Returns the byte length of the string. See `count()` to obtain the number of runes.
    @host fn len(self) int

    --| Returns whether this string is lexicographically before `other`.
    @host fn less(self, other string) bool

    --| Returns this string in lowercase.
    @host fn lower(self) string

    --| Returns a new string with all occurrences of `needle` replaced with `replacement`.
    @host fn replace(self, needle string, replacement string) string

    --| Returns a new string with this string repeated `n` times.
    @host fn repeat(self, n int) string

    --| Returns the starting byte index for the rune index `idx`.
    @host fn seek(self, idx int) int

    --| Returns the UTF-8 rune starting at byte index `idx` as a string.
    @host fn sliceAt(self, idx int) string

    --| Returns a list of UTF-8 strings split at occurrences of `sep`.
    @host fn split(self, sep string) List[string]

    --| Returns whether the string starts with `prefix`.
    @host fn startsWith(self, prefix string) bool

    --| Returns the string with ends trimmed from runes in `delims`. `mode` can be .left, .right, or .ends.
    @host fn trim(self, mode symbol, delims string) string

    --| Returns this string in uppercase.
    @host fn upper(self) string

@host fn string.interpolate(strs List[string], args List[any]) string

--| Converts a value to a string.
@host fn string.$call(val any) string

type StringIterator:
    str string
    idx int

    fn next(self) ?int:
        if self.idx >= self.str.len():
            return none
        var res = self.str[self.idx]
        self.idx += 1
        return res

@host
type Array[N int, T type] _:
    fn $indexAddr(self, idx int) *T:
        return self.indexAddr(N, typeid[T], idx)

    @host -fn indexAddr(self, n int, elem_t int, idx int) *T

    --| Returns a slice into this array from a `Range` with `start` (inclusive) to `end` (exclusive) indexes.
    fn $slice(self, start int, end int, has_end bool) []T:
        return self.slice(typeid[[]T], N, start, if (has_end) end else N)

    @host -fn slice(self, slice_t int, n int, start int, end int) []T

    -- @host fn '$infix+'(self, o Array[#M, T]) Array[N + M, T]

    -- --| Returns a new array that concats this array and `other`.
    -- @host fn concat(self, other Array[#M, T]) Array[N + M, T]

    -- --| Returns a new array with `arr` inserted at index `idx`.
    -- @host fn insert(self, idx int, arr Array) Array

    --| Returns a new iterator over the array.
    fn iterator(self) SliceIterator[T]:
        var slice = self[..]
        return slice.iterator()

    --| Returns the number of elements in the array.
    fn len(self) int:
        return N

    -- --| Returns a new array with this array repeated `n` times.
    -- @host fn repeat(self, #M int) Array[N * M, T]

@host
type pointer[T type] _:
    fn $indexAddr(self Self, idx int) *T:
        return self.indexAddr(typeid[T], idx)

    @host -fn indexAddr(self Self, elem_t int, idx int) *T

    fn $slice(self Self, start int, end int, has_end bool) [*]T:
        return self.slice(typeid[[*]T], start, end)

    @host -fn slice(self Self, slice_t int, start int, end int) [*]T

    fn $setIndex(self Self, idx int, val T) void:
        self.setIndex(typeid[T], idx, val)

    @host -fn setIndex(self Self, elem_t int, idx int, val T) void

    --| When pointer runtime safety is enabled, this returns the raw pointer address as an `int64`. 
    --| Otherwise, the pointer itself is bitcasted to an `int64`.
    @host fn addr(self Self) int

    --| Casts the pointer to a Cyber object. The object is retained before it's returned.
    @host fn asObject(self Self) any

    --| Returns a `string` from a null terminated C string.
    @host fn fromCstr(self Self, offset int) string

    --| Dereferences the pointer at a byte offset and returns the C value converted to Cyber.
    @host fn get(self Self, offset int, ctype symbol) dyn

    --| Returns a `string` with a copy of the byte data starting from an offset to the specified length.
    @host fn getString(self Self, offset int, len int) string

    --| Converts the value to a compatible C value and writes it to a byte offset from this pointer.
    @host fn set(self Self, offset int, ctype symbol, val any) void

    --| Sets the value assuming the underlying memory is uninitialized.
    -- fn init(self Self, val T) void:
    --     self.init_(typeid[T], val)

    -- @host fn init_(self Self, type_id int, val T) void

--| Converts an `int` to a `pointer` value.
@host fn pointer.fromAddr[T](T type, addr int) *T

--| Converts `^T` to `*T`.
@host fn pointer.fromRef[T](ref ^T) *T

@host
type ExternFunc _:
    --| Returns the memory address as an `int`. The value may be negative since it's
    --| bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.
    @host fn addr(self) int

    @host fn ptr(self) *void

@host
type Fiber _:
    @host fn status(self) symbol

@host
type Range struct:
    start int
    end   int

@host type TccState _

@host type Option[T type] _

@host type Future[T type] _

--| Returns a `Future[T]` that has a completed value.
fn Future.complete[T](val T) Future[T]:
    return Future.complete_(val, typeid[Future[T]])
    
@host -fn Future.complete_[T](val T, ret_type int) Future[T]

fn Future.new[T](T type) Future[T]:
    return Future.new_(T, typeid[Future[T]])

@host -fn Future.new_[T](T type, ret_type int) Future[T]

@host type FutureResolver[T type] _:
    @host fn complete(self, val T) void
    @host fn future(self) Future[T]

fn FutureResolver.new[T](T type) FutureResolver[T]:
    return FutureResolver.new_(T, typeid[Future[T]], typeid[FutureResolver[T]])

@host -fn FutureResolver.new_[T](T type, future_t int, ret_t int) FutureResolver[T]

@host
type ref[T type] _

type Slice[T type]:
    ptr ^T
    n   int

    fn $indexAddr(self Self, idx int) *T:
        return pointer.fromRef(self.ptr)[idx]

    -- fn $index(self, range Range) []T:
    --     var ptr_slice = self.ptr[range.start..range.end]
    --     return .{ ptr=ptr_slice.ptr, n=ptr_slice.n }

    fn $setIndex(self Self, idx int, val T) void:
        pointer.fromRef(self.ptr)[idx] = val

    -- --| Returns whether the array ends with `suffix`.
    -- @host fn endsWith(self, suffix []T) bool

    -- --| Returns the first index of `needle` in the slice or `none` if not found.
    -- @host fn find(self, needle []T) ?int

    -- --| Returns the first index of any `bytes` in `set` or `none` if not found.
    -- @host fn findAny(self, set []T) ?int

    -- --| Returns the first index of `needle` in the slice or `none` if not found.
    -- @host fn findScalar(self, needle T) ?int

    fn iterator(self Self) SliceIterator[T]:
        return SliceIterator[T]{slice=self, next_idx=0}

    fn len(self Self) int:
        return self.n

    -- --| Returns a list of arrays split at occurrences of `sep`.
    -- @host fn split(self, sep []T) List[Array]

    -- --| Returns whether the array starts with `prefix`.
    -- @host fn startsWith(self, prefix []T) bool

    -- --| Returns a slice with ends trimmed from `delims`. `mode` can be .left, .right, or .ends.
    -- @host fn trim(self, mode symbol, delims []T) []T

type SliceIterator[T type]:
    slice    []T
    next_idx int

    fn next(self) ?T:
        if self.next_idx >= self.slice.len():
            return none
        var res = self.slice[self.next_idx]
        self.next_idx += 1
        return res

type PtrSlice[T type] struct:
    ptr *T
    n   int

    fn $indexAddr(self Self, idx int) *T:
        return self.ptr[idx]

    fn $slice(self Self, start int, end int, has_end bool) [*]T:
        return self.ptr[start..(if (has_end) end else self.n)]

    fn $setIndex(self Self, idx int, val T) void:
        self.ptr[idx] = val

    --| Returns whether the array ends with `suffix`.
    fn endsWith(self Self, suffix [*]T) bool:
        if suffix.len() > self.len():
            return false
        for self[self.len()-suffix.len()..] -> elem, i:
            if elem != suffix[i]:
                return false
        return true

    fn fill(self Self, val T) void:
        for 0..self.n -> i:
            self.ptr[i] = val

    -- --| Returns the first index of `needle` in the slice or `none` if not found.
    -- @host fn find(self, needle [*]T) ?int

    -- --| Returns the first index of any `bytes` in `set` or `none` if not found.
    -- @host fn findAny(self, set [*]T) ?int

    --| Returns the first index of `needle` in the slice or `none` if not found.
    fn findScalar(self Self, needle T) ?int:
        for self -> elem, i:
            if elem == needle: return i
        return none

    fn iterator(self Self) PtrSliceIterator[T]:
        return PtrSliceIterator[T]{slice=self, next_idx=0}

    fn len(self Self) int:
        return self.n

    -- TODO: Consider returning iterator instead.
    -- --| Returns a list of arrays split at occurrences of `sep`.
    -- @host fn split(self, sep [*]T) List[Array]

    --| Returns whether the array starts with `prefix`.
    fn startsWith(self Self, prefix [*]T) bool:
        if prefix.len() > self.len():
            return false
        for self[..prefix.len()] -> elem, i:
            if elem != prefix[i]:
                return false
        return true

    -- --| Returns a slice with ends trimmed from `delims`. `mode` can be .left, .right, or .ends.
    -- @host fn trim(self, mode symbol, delims [*]T) [*]T

type PtrSliceIterator[T type]:
    slice    [*]T
    next_idx int

    fn next(self) ?T:
        if self.next_idx >= self.slice.len():
            return none
        var res = self.slice[self.next_idx]
        self.next_idx += 1
        return res

type IMemory trait:
    fn alloc(self, len int) [*]byte
    fn free(self, buf [*]byte) void

@host
type Memory:
    iface IMemory

fn Memory.new[T](self, T type) *T:
    var bytes = self.iface.alloc(sizeof(T))
    var ptr = ptrcast(T, bytes.ptr)
    return ptr

fn Memory.alloc[T](self, T type, n int) [*]T:
    var bytes = self.iface.alloc(sizeof(T) * n)
    var slice = ptrcast(T, bytes.ptr)[0..n]
    return slice

fn Memory.free[T](self, ptr *T):
    self.iface.free(ptrcast(byte, ptr)[0..sizeof(T)])

-- TODO: Should be merged with Memory.free.
fn Memory.free_[T](self, slice [*]T):
    var bytes = ptrcast(byte, slice.ptr)[0..sizeof(T)*slice.len()]
    self.iface.free(bytes)

type DefaultMemory:
    with IMemory

    @host fn alloc(self, len int) [*]byte
    @host fn free(self, buf [*]byte) void

@host type ExprType type:
    @host fn getType(self) type
