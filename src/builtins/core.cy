use meta

--| This module is imported into each module's using namespace.
--| Contains builtin types/functions and common utilities.

--| Sample usage:
--| ```cy
--| -- `print` is available without additional imports.
--| print('hello')
--| ```

--|
--| Functions.
--|

--| Terminate program with an error message.
#[bind]
fn abort(err str) -> never

fn alloc(%T type) -> Ptr[T]:
    return as alloc_(type.size(T))

--| Unsafe. Allocate memory using the default allocator.
fn alloc(%T type, n int) -> PtrSpan[T]:
    if n == 0:
        return {
            ptr = none,
            length = 0,
        }
    ptr := alloc_(type.size(T) * n)
    return (as[Ptr[T]] ptr)[0..n]

#[bind]
-fn alloc_(len int) -> Ptr[byte]

fn assert(pred bool):
    if !pred:
        panic('Assertion failed.')

fn assert(exp %T, act T):
    if exp != act:
        panic('Expected `%{exp}`, found `%{act}`.')

--| Assumes a sorted span in ascending order.
fn binary_search(len int, needle %T, compare &OpaqueFunc[CompareIndexFn[T]]) -> ?int:
    low := 0
    high := len
    while low < high:
        mid := low + (high - low) / 2
        switch compare.*(needle, mid):
            case .eq: return mid
            case .gt: low = mid + 1
            case .lt: high = mid
    return none

--| Assumes a sorted span in ascending order.
--| Returns the lowest index where the element at the index 
--| is greater than or equal to the needle.
--| If no such element exists, the index after the last element is returned.
fn binary_search_lower(len int, needle %T, compare &OpaqueFunc[CompareIndexFn[T]]) -> int:
    if len == 0:
        return 0
    low := 0
    high := len
    while low < high:
        mid := low + (high - low) / 2
        if compare.*(needle, mid) == .gt:
            low = mid + 1
        else:
            high = mid
    return low

--| Deep dump of a value.
fn dump(val %T):
    log(dump_str(move val))

fn dump_str(val %T) -> str:
    return dump_str(&val, 0)

fn dump_str(val &%T, indent int) -> str:
    #if T == str:
        return "str '%{val.*}'"
    #else type.is_instance_of(T, Slice):
        return '%{type.name(T)}(%{val.len()})'
    #else:
        #switch type.info(T):
            #case .struct |info|:
                out := '%{type.name(T)}{\n'
                #for 0..info.fields.len() |i|:
                    #field := info.fields[i]
                    out += '  '.repeat(indent+1)
                    out += '%{#{field.name}}: %{dump_str(&meta.access(val, field.name), indent+1)}\n'
                out += '  '.repeat(indent)
                out += '}'
                return out

            #case .option |info|:
                if meta.is_none(val):
                    return '%{type.name(T)}(none)'
                else:
                    out := '?'
                    out += dump_str(&meta.access_option_payload(val.*), indent)
                    return out

            #case .choice |info|:
                out := '%{type.name(T)}.%{meta.enum_name(meta.choice_tag(val.*))}('
                switch meta.choice_tag(val.*):
                    #for meta.enum_values(T.Tag) |Tag|:
                        case Tag:
                            out += dump_str(&meta.access_choice_case(val.*, Tag), indent)
                out += ')'
                return out

            #case .ref |info|:
                -- Skip dereferencing since that would need a dedupe cache.
                return '%{type.name(T)}'

            #case .ptr |info|:
                return '%{type.name(T)} %{val.*}'

            #case .vector |info|:
                children := []str{}
                for 0..#{info.len} |i|:
                    children <<= dump(val[i])
                return '{%{children.join(', ')}}'

            #else:
                return '%{type.name(T)}'

--| Prints a value to the error stream.
fn eprint(x %T):
    eprints(x)
    eprints_('\n')

fn eprints(x %T):
    eprints_(to_print_string(x))

#[bind]
-fn eprints_(s str)

--| Unsafe. Frees memory allocated by the default allocator.
#[bind] fn free(ptr Ptr[void])

--| Builds a stack trace and aborts the current thread.
#[bind] fn panic(msg str) -> never

-fn panic_oob(x int):
    panic('Out of bounds index: %{x}')

--| Logs a value to the host. Thread-safe.
fn log(x %T):
    log_(to_print_string(x))
    
#[bind]
-fn log_(s str)

--| Prints a value to the output stream. Not thread-safe.
fn print(x %T):
    prints(x)
    prints_('\n')

fn prints(x %T):
    prints_(to_print_string(x))

#[bind]
-fn prints_(s str)

fn to_print_string(x %T) -> str:
    #if T == bool:
        return str(x)
    #else T == i8:
        return str(x)
    #else T == int:
        return str(x)
    #else T == i32:
        return str(x)
    #else T == i16:
        return str(x)
    #else T == byte:
        return str(x)
    #else T == r16:
        return str(x)
    #else T == r32:
        return str(x)
    #else T == r64:
        return str(x)
    #else T == float:
        return str(x)
    #else T == f32:
        return str(x)
    #else T == error:
        return str(x)
    #else T == symbol:
        return str(x)
    #else T == str:
        return x
    #else:
        #switch type.info(T):
            #case .enum:
                return type.name(T) + '.' + meta.enum_name(x)
            #case .option:
                if x |payload|:
                    return to_print_string(payload)
                else:
                    return 'none'
            #case .choice:
                return type.name(T) + '@' + meta.enum_name(meta.choice_tag(x)) + '{}'
            #case .ptr |ptr|:
                return '@' + (as[r64] x).fmt()
            #case .func |func|:
                #if func.kind == .ptr:
                    return '@' + (as[r64] x).fmt()
                #else:
                    return type.name(T)
            #else:
                return type.name(T)

#[bind] fn @compute_int_max(%T type) -> T
#[bind] fn @compute_int_min(%T type) -> T

fn panic_unwrap_error(err error) -> never:
    panic('Unwrapped error `%{err}`.')

--|
--| Types.
--|

-- Type naming convention:
-- 1. Primitives and immutable value types start with a lower case letter.
-- 2. All other types start with a capital letter.

#[bind] type void _

#[bind] type never _

#[bind] type bool _

fn bool :: @init(val int) -> bool:
    return val != 0

fn bool :: @init(val float) -> bool:
    return val != 0

fn bool :: @init(val str) -> bool:
    return val == 'true'

#[reserve]
fn (bool) `!`() -> bool

#[bind] type symbol _

#[bind] fn (symbol) name() -> str

#[bind] type error _

--| Create an error from symbol.
fn error :: @init(val symbol) -> error:
    return as val

--| Return the underlying `symbol`.
fn (error) sym() -> symbol:
    return as $

#[bind] type EvalInt _

--| Const eval string.
#[bind]
type EvalStr _

fn EvalStr :: @init(x Self) -> Self:
    return x

#[bind]
fn EvalStr :: @init(x %T) -> Self

#[bind]
fn (EvalStr) `+`(o Self) -> Self

type NumberFormatConfig:
    --| `pad` provides the ASCII rune that is used for padding.
    pad   ?int

    --| `width` maximum width.
    width ?int

type byte = Raw[8]
type r8  = Raw[8]
type r16 = Raw[16]
type r32 = Raw[32]
type r64 = Raw[64]
type rsize = if (meta.pointer_width() == 64) r64 else r32

--| Represents a raw integer value with bits of widths 8, 16, 32, or 64.
#[bind] type Raw[const Bits EvalInt] _

const Raw[] :: zero Self = 0
const Raw[] :: ones Self = ~0

fn Raw[] :: @init(x Self) -> Self:
    return x

fn Raw[] :: @init(x %T) -> Self:
    #if T == bool:
        return if (x) 1 else 0
    #else T == float:
        res := @floatToInt(f32, x)
        if res.uge(Self.umax):
            panic('Lossy conversion to `%{type.name(Self)}`.')
        #if Bits == 64:
            return @bitCast(Self, res)
        #else:
            return @bitTrunc(Self, res)
    #else T == f32:
        res := @floatToInt(f32, x)
        if res.uge(Self.ones):
            panic('Lossy conversion to `%{type.name(Self)}`.')
        #if Bits == 64:
            return @bitCast(Self, res)
        #else:
            return @bitTrunc(Self, res)
    #else:
        #switch type.info(T):
            #case .int |int_t|:
                #if int_t.bits == Bits:
                    return @bitCast(Self, x)
                #else int_t.bits > Bits:
                    if (x >> Bits) == 0:
                        return @bitTrunc(Self, x)
                    else:
                        panic('Lossy conversion to `%{type.name(Self)}`.')
                #else:
                    return @zext(Self, x)
            #else:
                meta.error('Unsupported: ' + type.name(T))

#[reserve] fn (Raw[]) `~`() -> Self
#[reserve] fn (Raw[]) `<`(right Self) -> bool
#[reserve] fn (Raw[]) `<=`(right Self) -> bool
#[reserve] fn (Raw[]) `>`(right Self) -> bool
#[reserve] fn (Raw[]) `>=`(right Self) -> bool
#[reserve] fn (Raw[]) `+`(right Self) -> Self
#[reserve] fn (Raw[]) `-`(right Self) -> Self
#[reserve] fn (Raw[]) `*`(right Self) -> Self
#[reserve] fn (Raw[]) `/`(right Self) -> Self
#[reserve] fn (Raw[]) `%`(right Self) -> Self
#[reserve] fn (Raw[]) `&&`(right Self) -> Self
#[reserve] fn (Raw[]) `||`(right Self) -> Self
#[reserve] fn (Raw[]) `~`(right Self) -> Self
#[reserve] fn (Raw[]) `<<`(right Self) -> Self
#[reserve] fn (Raw[]) `>>`(right Self) -> Self

fn (Raw[]) fmt() -> str:
    return $fmt(.hex)

--| Formats the integer (interpreted as unsigned) using a NumberFormat.
fn (Raw[]) fmt(format NumberFormat) -> str:
    switch format:
        case .bin:
            return '0b' + raw_fmt($, format)
        case .oct:
            return '0o' + raw_fmt($, format)
        case .hex:
            return '0x' + raw_fmt($, format)
        else:
            return raw_fmt($, format)

#[bind] fn raw_fmt(x r64, format NumberFormat) -> str

type i8  = Int[8]
type i16 = Int[16]
type i32 = Int[32]
type i64 = Int[64]
type int = Int[64]
type isize = if (meta.pointer_width() == 64) i64 else i32

--| A two's complement signed integer for bit widths: 8, 16, 32, and 64.
#[bind] type Int[const Bits EvalInt] _

const Int[] :: min = @compute_int_min(Self)
const Int[] :: max = @compute_int_max(Self)

const Int[] :: umin Self = 0
const Int[] :: umax Self = -1

fn Int[] :: @init(x Self) -> Self:
    return x

#[bind]
fn Int[] :: @init(x %T) -> Self:
    #if T == str:
        return Self.parse(x) !else 0
    #else T == bool:
        return if (x) 1 else 0
    #else T == float:
        i_64 := @floatToInt(float, x)
        return Self(i_64)
    #else T == f32:
        i_64 := @floatToInt(f32, x)
        return Self(i_64)
    #else:
        #switch type.info(T):
            #case .int |int_t|:
                #if int_t.bits < Bits:
                    return @sext(Self, x)
                #else int_t.bits > Bits:
                    if x > Self.max or x < Self.min:
                        panic('Lossy conversion from `%{x}` to `%{type.name(Self)}`')
                    return @bitTrunc(Self, x)
                #else:
                    return x
            #else:
                meta.error('Unsupported: ' + type.name(T))

fn Int[] :: unsigned(x %T) -> Self:
    #if T == bool:
        return if (x) 1 else 0
    #else T == float:
        i_64 := @floatToInt(float, x)
        return Self.unsigned(i_64)
    #else T == f32:
        i_64 := @floatToInt(f32, x)
        return Self.unsigned(i_64)
    #else:
        #switch type.info(T):
            #case .int |int_t|:
                -- Interprets `x` int as unsigned.
                #if int_t.bits == Bits:
                    return x
                #else int_t.bits > Bits:
                    if (x >> Bits) == 0:
                        return @bitTrunc(Self, x)
                    else:
                        panic('Lossy conversion to `%{type.name(Self)}`.')
                #else:
                    return @zext(Self, x)
            #else:
                meta.error('Unsupported: ' + type.name(T))

fn Int[] :: decode(ptr Ptr[byte]) -> Self:
    return Self.decode(ptr, .little)

--| Returns the integer value starting from the pointer with the given endian.
fn Int[] :: decode(ptr Ptr[byte], endian Endian) -> Self:
    switch endian:
        case .little:
            res := Self(0)
            for 0..Bits/8 |i|:
                res = res || (@zext(Self, ptr[i]) << (Self(i) * 8))
            return res
        case .big:
            res := Self(0)
            for 0..Bits/8 |i|:
                res = (res << 8) || @zext(Self, ptr[i])
            return res

fn Int[] :: parse(s str) -> !Self:
    slen := s.len()
    if slen == 0:
        return 0

    res := Self(0)
    exp := Self(0)
    for slen-1..>=0 |i|:
        ch := s[i]
        if ch == '-':
            res = -res
            if i != 0:
                return error.InvalidCharacter
        if !str.is_ascii_digit(ch):
            return error.InvalidCharacter
        digit := Self(ch - '0')
        res += digit * 10**exp
        exp += 1
    return res

#[reserve] fn (Int[]) `~`() -> Self
#[reserve] fn (Int[]) `-`() -> Self
#[reserve] fn (Int[]) `<`(right Self) -> bool
#[reserve] fn (Int[]) `<=`(right Self) -> bool
#[reserve] fn (Int[]) `>`(right Self) -> bool
#[reserve] fn (Int[]) `>=`(right Self) -> bool
#[reserve] fn (Int[]) `+`(right Self) -> Self
#[reserve] fn (Int[]) `-`(right Self) -> Self
#[reserve] fn (Int[]) `*`(right Self) -> Self
#[reserve] fn (Int[]) `/`(right Self) -> Self
#[reserve] fn (Int[]) `%`(right Self) -> Self
#[reserve] fn (Int[]) `&&`(right Self) -> Self
#[reserve] fn (Int[]) `||`(right Self) -> Self
#[reserve] fn (Int[]) `~`(right Self) -> Self
#[reserve] fn (Int[]) `<<`(right Self) -> Self
#[reserve] fn (Int[]) `>>`(right Self) -> Self

fn (Int[]) ult(right Self) -> bool:
    return (as[Raw[Bits]] self) < (as[Raw[Bits]] right)

fn (Int[]) ule(right Self) -> bool:
    return (as[Raw[Bits]] self) <= (as[Raw[Bits]] right)

fn (Int[]) ugt(right Self) -> bool:
    return (as[Raw[Bits]] self) > (as[Raw[Bits]] right)

fn (Int[]) uge(right Self) -> bool:
    return (as[Raw[Bits]] self) >= (as[Raw[Bits]] right)

fn (Int[]) umul(right Self) -> Self:
    return (as[Raw[Bits]] self) * (as[Raw[Bits]] right)

fn (Int[]) udiv(right Self) -> Self:
    return (as[Raw[Bits]] self) / (as[Raw[Bits]] right)

fn (Int[]) umod(right Self) -> Self:
    return (as[Raw[Bits]] self) % (as[Raw[Bits]] right)

fn (Int[]) asr(right Self) -> Self:
    return @asr(self, right)

#[bind]
fn @asr(x Int[%Bits], amt Int[Bits])

fn (Int[]) `**`(e Self) -> Self:
    b := $
    if e < 0:
        if b == 1 and e == -1:
            return 1
        if b == -1 and e == -1:
            return  -1
        return 0

    result := 1
    while:
        if e && 1 > 0:
            result *= b
        e >>= 1
        if e == 0:
            break
        b *= b
    return result

fn (Int[]) fmt() -> str:
    return $fmt(.dec)

--| Formats the integer using a NumberFormat.
fn (Int[]) fmt(format NumberFormat) -> str:
    return int_fmt($, format)

-- --| `opts.pad` provides the ASCII rune that is used for padding with a string length of `config.width`.
-- #[bind='int.fmt2']
-- fn Int.fmt(self, format NumberFormat, config NumberFormatConfig) -> str:
--     panic('Unsupported')

#[bind] fn int_fmt(x int, format NumberFormat) -> str

fn (Int[]) ufmt() -> str:
    return $ufmt(.dec)

--| Formats the integer (interpreted as unsigned) using a NumberFormat.
fn (Int[]) ufmt(format NumberFormat) -> str:
    return (as[r64] $).fmt(format)

#[bind] fn @bitTrunc(%T type, src Code) -> T

--| Sign extension.
#[bind] fn @sext(%T type, src %S) -> T

--| Zero extension.
#[bind] fn @zext(%T type, src %S) -> T

#[bind] fn @floatToInt(%F type, x Code) -> int
#[bind] fn @intToFloat(%F type, x Code) -> F

type Endian enum:
    case little
    case big

type NumberFormat enum:
    --| UTF-8 character.
    case ch
    --| binary
    case bin
    --| decimal
    case dec
    --| hexadecimal
    case hex
    --| octal
    case oct

type f32   = Float[32]
type f64   = Float[64]
type float = Float[64]

#[bind] type Float[const Bits EvalInt] _

fn Float[] :: @init(x Self) -> Self:
    return x

fn Float[] :: @init(x int) -> Self:
    return @intToFloat(Self, x)

fn Float[] :: @init(x %T) -> Self:
    #if T == bool:
        return if (x) 1 else 0
    #else T == f32:
        return f32_to_f64(x)
    #else T == float:
        return f64_to_f32(x)
    #else T == str:
        #if Bits == 64:
            return atof(x)
        #else:
            return atof32(x)
    #else:
        meta.unsupported()

#[bind] fn atof(x str) -> float
#[bind] fn atof32(x str) -> f32
#[bind] fn float_pow(x, y float) -> float
#[bind] fn float_pow32(x, y float) -> f32
#[bind] fn f32_to_f64(x f32) -> float
#[bind] fn f64_to_f32(x float) -> f32
#[bind] fn f64_fmt(x float) -> str
#[bind] fn f32_fmt(x f32) -> str

#[reserve] fn (Float[]) `-`() -> Self
#[reserve] fn (Float[]) `<`(right Self) -> bool
#[reserve] fn (Float[]) `<=`(right Self) -> bool
#[reserve] fn (Float[]) `>`(right Self) -> bool
#[reserve] fn (Float[]) `>=`(right Self) -> bool
#[reserve] fn (Float[]) `+`(right Self) -> Self
#[reserve] fn (Float[]) `-`(right Self) -> Self
#[reserve] fn (Float[]) `*`(right Self) -> Self
#[reserve] fn (Float[]) `/`(right Self) -> Self
#[reserve] fn (Float[]) `%`(right Self) -> Self
fn (Float[]) `**`(right Self) -> Self:
    #if Bits == 64:
        return float_pow($, right)
    #else:
        return float_pow32($, right)

fn (Float[]) abs() -> Self:
    return @fabs(self)

#[bind] fn @fabs(x Float[%Bits]) -> Float[Bits]

fn (Float[]) fmt() -> str:
    #if Self == float:
        return f64_fmt($)
    #else Self == f32:
        return f32_fmt($)
    #else:
        meta.unsupported()

#[bind]
type Object:
    inner Ptr[void]

fn Object :: @init(ref ^%T) -> Object:
    -- Ensure @deinitObject is expanded for `T`.
    _ = @deinitObject[T]
    @retain(as ref)
    return {inner=as ref}

fn (&Object) @copy() -> Self:
    @retain($inner)
    return {
        inner = $inner,
    }

fn (&Object) @deinit():
    if @releaseOnly($inner):
        @destructBox($inner)

fn (&Object) downcast(%Ref type) -> Ref:
    #exp_t := type.info(Ref).!ref.child
    if $type() == type.id(exp_t):
        @retain($inner)
        return as $inner
    else:
        panic('Downcast expected `%{type.name(Ref)}` typeid=%{type.id(exp_t)}, found typeid=%{$type()}.')
    
#[bind] fn (&Object) type() -> int

fn (&Object) toString() -> str:
    obj_type := $type()
    switch obj_type:
        case type.id(bool):
            value := (as[^bool] $inner).*
            return if (value) 'true' else 'false'
        case type.id(r8)
        case type.id(r16)
        case type.id(r32)
        case type.id(r64):
            return (as[^r64] $inner).*.fmt()
        case type.id(i8)
        case type.id(i16)
        case type.id(i32)
        case type.id(int):
            return (as[^int] $inner).*.fmt()
        case type.id(float):
            return (as[^float] $inner).*.fmt()
        case type.id(error):
            return 'error.' + (as[^error] $inner).*.sym().name()
        case type.id(symbol):
            return '@' + (as[^symbol] $inner).*.name()
        case type.id(str):
            return (as[^str] $inner).*
        else:
            return 'Object'

type TaskCallback = fn() -> void

#[bind] type Any _
#[bind] type dependent _
#[bind] type Infer _

type type = meta.type
type PartialStructLayout = meta.PartialStructLayout
type Code = meta.Code

--| Can only be created as an object.
--| Unsafe because elements are not guaranteed to be initialized.
type RawBuffer[T Any]:
    length int
    data   void

fn RawBuffer[] :: @init(nelems int) -> ^Self:
    ptr := @new_object_undef(type.id(Self), 8 + nelems * type.size(T))
    (as[Ptr[Self]] ptr).length = nelems
    return as move ptr

fn (&RawBuffer[]) @size() -> int:
    return 8 + $length * type.size(T)

fn (&RawBuffer[]) elems() -> PtrSpan[T]:
    return (as[Ptr[T]] *$data)[0..$length]

fn (&RawBuffer[]) elems_ptr() -> Ptr[T]:
    return as *$data

fn (&RawBuffer[]) len() -> int:
    return $length

fn (&RawBuffer[]) @deinit():
    #if type.managed(T):
        buf := as[Ptr[T]] *$data
        for 0..$length |i|:
            @destruct(T, buf + i)

--| An implicit trait that describes a type that allows implicit copying.
--| A type is `Copyable` if these two conditions are met:
--| 1. All its members are `Copyable` or the `@copy` method is implemented.
--| 2. The type does not also implement `NoCopy`.
type Copyable trait

fn Copyable :: @predicate(T type) -> bool:
    return type.is_copyable(T)

--| Describes a type that forbids implicit copying disregarding whether the type is implicitly `Copyable`.
#[bind]
type NoCopy trait

type ExDefaultInner[T Any]:
    child T
    ex_borrow bool
    borrow bool

type ExInner[T Any] const:
    if meta.has_decl(T, '$acquire_ex_borrow'):
        return T
    else:
        return ExDefaultInner[T]

--| Exclusive borrow container.
--| Allows exclusive borrowing to a child `T` in a shared reference `^Ex[T]`.
--| Defers borrow checking to runtime.
type Ex[T Any]:
    inner ExInner[T]

fn (&Ex[]) ex_borrow() -> RtExBorrow[T]:
    -- TODO: Check for trait instead.
    #if meta.has_decl(T, '$acquire_ex_borrow'):
        if !$inner.@acquire_borrow():
            panic('Cannot acquire exclusive borrow.')
        return @unsafeCast(&&T, &$inner)
    #else:
        if $inner.ex_borrow:
            panic('Cannot acquire exclusive borrow.')
        $inner.ex_borrow = true
        return @unsafeCast(&&T, &inner.child)

fn (&Ex[]) borrow() -> RtBorrow[T]:
    #if meta.has_decl(T, '$acquire_ex_borrow'):
        switch $inner.@acquire_borrow():
            case @ex_borrowed:
                panic('Cannot acquire borrow.')
            case @borrowed:
                return {
                    ptr = &$inner,
                    acquired = false,
                }
            case @acquired:
                return {
                    ptr = &$inner,
                    acquired = true,
                }
    #else:
        if $inner.ex_borrow:
            panic('Cannot acquire borrow.')
        if $inner.borrow:
            return {
                ptr = &$inner.child,
                acquired = false,
            }
        else:
            $inner.borrow = true
            return {
                ptr = &$inner.child,
                acquired = true,
            }

type AcquireBorrowKind enum:
    case acquired
    case borrowed
    case ex_borrowed

type RtExBorrow[T Any]:
    ptr Ptr[T]

fn (&RtExBorrow[]) @deinit():
    #if meta.has_decl(T, '$acquire_ex_borrow'):
        $inner.@release_ex_borrow()
    #else:
        (as[ExDefaultInner[T]] $ptr).ex_borrow = false

type RtBorrow[T Any]:
    ptr  Ptr[T]
    acquired bool

fn (&RtBorrow[]) @deinit():
    if !$acquired:
        return
    #if meta.has_decl(T, '$acquire_ex_borrow'):
        $ptr.@release_borrow()
    #else:
        (as[ExDefaultInner[T]] $ptr).borrow = false

#[bind]
type EvalBuffer[T Any] _

#[bind]
fn (EvalBuffer[]) @index_addr(idx int) -> Ptr[T]

#[bind]
fn (EvalBuffer[]) len() -> int

#[consteval]
fn (EvalBuffer[]) iterator() -> int:
    return 0

#[consteval]
fn (EvalBuffer[]) next(idx &int) -> ?T:
    if idx.* == self.len():
        return none

    val := self[idx.*]
    idx.* += 1
    return val

--| A data structure that holds sequential elements up to a maximum capacity.
--| `len()` indicates which elements are initialized.
--| `cap()` is the total number of elements the buffer can hold.
type Buffer[T Any]:
    with NoCopy
    with AsSpan[T]

    base   Ptr[T]
    length int

    -- 2-bit MSB is reserved for `Ex` borrowing.
    -- The rest represents the capacity.
    header int

fn Buffer[] :: @init_sequence(init [&]T) -> Self:
    if init.length == 0:
        return {
            base   = none,
            length = 0,
            header = 0,
        }
    base := alloc(T, init.length).ptr
    base[0..init.length].init(as init)
    return {
        base   = base,
        length = init.length,
        header = init.length,
    }

fn Buffer[] :: @init(nelems int) -> Self:
    base := alloc(T, nelems * type.size(T)).ptr
    return {
        base = base,
        length = 0,
        header = nelems,
    }

fn Buffer[] :: @init(n int, value T) -> Self:
    base := alloc(T, n * type.size(T)).ptr
    base[0..n].init(value)
    return {
        base = base,
        length = n,
        header = n,
    } 

fn (&Buffer[]) @acquire_ex_borrow() -> bool:
    if ($header && (1 << 63)) == 0:
        $header ||= 1 << 63
        return true
    return false

fn (&Buffer[]) @release_ex_borrow():
    $header &&= ~(3 << 62)

fn (&Buffer[]) @acquire_borrow() -> AcquireBorrowKind:
    if ($header && (1 << 63)) != 0:
        return @ex_borrowed
    if ($header && (1 << 62)) == 0:
        $header ||= 1 << 62
        return @acquired
    return @borrowed

fn (&Buffer[]) @release_borrow():
    $header &&= ~(3 << 62)

fn (scope &Buffer[]) @index_addr(idx int) -> scope &T:
    if idx.uge($length):
        panic_oob(idx)
    return @scope_ptr_to_borrow('self', self.base + idx)

fn (&&Buffer[]) `<<`(elem T):
    if $length == $cap():
        panic('No more buffer space.')
    @ptr_init($base + $length, elem)
    $length += 1

fn (scope &Buffer[]) span() -> scope [&]T:
    return $[..]

fn (scope &Buffer[]) @slice() -> scope [&]T:
    return {
        base   = @scope_ptr_to_borrow('self', self.base),
        length = $length,
    }

fn (scope &Buffer[]) @slice(start int) -> scope [&]T:
    if start > $length:
        panic_oob(start)
    return {
        base   = @scope_ptr_to_borrow('self', self.base + start),
        length = $length - start,
    }

fn (scope &Buffer[]) @slice(start int, end int) -> scope [&]T:
    if start > $length:
        panic_oob(start)
    if end > $length:
        panic_oob(end)
    if end < start:
        panic('InvalidArgument')
    return {
        base   = @scope_ptr_to_borrow('self', self.base + start),
        length = end - start,
    }

fn (&Buffer[]) @deinit():
    #if type.managed(T):
        for 0..$length |i|:
            @destruct(T, $base + i)
    free($base)

fn (&Buffer[]) cap() -> int:
    return $header && ~(3 << 62)

fn (&Buffer[]) iterator() -> int:
    return 0

fn (&Buffer[]) last() -> T:
    return $[$length-1]

fn (&Buffer[]) len() -> int:
    return $length

fn (&Buffer[]) next(idx &int) -> ?T:
    if idx.* == $length:
        return none

    val := $[idx.*]
    idx.* += 1
    return val

--| Decreases the size of the Buffer to `new_len` elements.
--| If the new size is smaller, elements at the end of the Buffer are removed.
fn (&&Buffer[]) size_down(new_len int):
    if new_len.ugt($length):
        panic('Invalid argument.')
    if new_len == $length:
        return

    #if type.managed(T):
        for new_len..$length |i|:
            @destruct(T, $buf + i)
    $length = new_len

--| A dynamically sized data structure that contains a sequence of elements.
--| Two copies initially point to the same element buffer and allow read/write to the elements.
--| However, any resize operation on a slice that is shared results in a clone of the underlying buffer.
--| Any resize operation on a sub-slice clones the underlying buffer regardless if it's shared.
type Slice[T Any]:
    with AsSpan[T]

    -- Elements stored as byte buffer to override destruction logic.
    -- TODO: Should just be managed as a rc pointer.
    buf     ?^RawBuffer[byte]
    ptr     Ptr[T]
    _len    int

    -- MSB tracks whether this is a sub-slice.
    -- The rest represents the buffer capacity.
    header  int

fn Slice[] :: @init_sequence(init [&]T) -> Self:
    if init.length == 0:
        return {
            buf    = none,
            ptr    = none,
            _len   = 0,
            header = 1 << 63,
        }
    cap := compute_new_cap(0, init.length)
    buf := new_slice_buffer(T, cap)
    elems_ptr := as[Ptr[T]] buf.elems_ptr()
    elems_ptr[0..init.length].init(as init)
    return {
        buf    = move buf,
        ptr    = elems_ptr,
        _len   = init.length,
        header = cap,
    }

fn new_slice_buffer(%T type, cap int) -> ^RawBuffer[byte]:
    ptr := @new_object_undef(type.id(RawBuffer[byte]), 8 + type.size(T) * cap)
    buf := as[^RawBuffer[byte]] ptr
    buf.length = type.size(T) * cap
    return move buf

--| Creates a `Slice` with initial length and capacity of `n` and values set to `val`.
fn Slice[] :: @init(n int, val T) -> Self:
    buf := new_slice_buffer(T, n)
    elems_ptr := as[Ptr[T]] buf.elems_ptr()
    elems_ptr[0..n].init(val)
    return {
        buf    = buf,
        ptr    = elems_ptr,
        _len   = n,
        header = n,
    }

fn (&Slice[]) @deinit():
    #if type.managed(T):
        buf := @unsafeCast(Ptr[RawBuffer[T]], $buf)
        if buf == none: return
        if !@isUniqueRef(buf): return

        if $header >> 63 != 0:
            base := as[Ptr[T]] buf.elems_ptr()
            -- `header` contains the active length for a sub-slice.
            for 0..$header && ~(1 << 63) |i|:
                @destruct(T, base + i)
        else:
            for 0..$_len |i|:
                @destruct(T, $ptr + i)

#[unsafe]
fn (&Slice[]) as_ptr_span() -> PtrSpan[byte]:
    return {
        ptr = $ptr,
        length = $_len,
    }

fn (scope &Slice[]) @index_addr(idx int) -> scope &T:
    if idx.uge($_len):
        panic_oob(idx)
    return @scope_ptr_to_borrow('self', as[Ptr[T]] $ptr + idx)

-- Old $index_addr kept for reference.
-- fn (&Slice[]) @index_addr(idx int) -> RefChild[PartialBuffer[T], T]:
--     if idx.uge($length):
--         panic_oob(idx)
--     return {
--         parent = $buf.?,
--         ptr    = $ptr + idx,
--     }

fn (&Slice[]) @index(idx int) -> T:
    if idx.uge($_len):
        panic_oob(idx)
    return $ptr[idx]

fn (&Slice[]) @set_index(idx int, val T):
    if idx.uge($_len):
        panic_oob(idx)
    $ptr[idx] = val

fn (&Slice[]) @slice() -> []T:
    if $header >> 63 != 0:
        -- sub-slice, preserve parent slice length in `header`.
        return {
            buf   = $buf,
            ptr   = $ptr,
            _len  = $_len,
            header = $header,
        }
    else:
        -- slice, return with sub-slice flag.
        return {
            buf   = $buf,
            ptr   = $ptr,
            _len  = $_len,
            header = $_len || (1 << 63),
        }

fn (&Slice[]) @slice(start int) -> []T:
    if start > $_len:
        panic_oob(start)

    if $header >> 63 != 0:
        -- sub-slice, preserve parent slice length in `header`.
        return {
            buf   = $buf,
            ptr   = $ptr + start,
            _len  = $_len - start,
            header = $header,
        }
    else:
        -- slice, return with sub-slice flag.
        return {
            buf   = $buf,
            ptr   = $ptr + start,
            _len  = $_len - start,
            header = $_len || (1 << 63),
        }

fn (&Slice[]) @slice(start int, end int) -> []T:
    if start > $_len:
        panic_oob(start)
    if end > $_len:
        panic_oob(end)
    if end < start:
        panic('InvalidArgument')
    if $header >> 63 != 0:
        -- sub-slice, preserve parent slice length in `header`.
        return {
            buf   = $buf,
            ptr   = $ptr + start,
            _len  = end - start,
            header = $header,
        }
    else:
        -- slice, return with sub-slice flag.
        return {
            buf   = $buf,
            ptr   = $ptr + start,
            _len  = end - start,
            header = $_len || (1 << 63),
        }

fn (&Slice[]) set(o Self):
    $span().set(o.span())

--| Alias for `append`.
fn (&Slice[]) `+`(val T) -> Self:
    return Self.append[0]($, val)

--| Alias for `append`.
fn (&Slice[]) `+`(slice [&]T) -> Self:
    return Self.append[1]($, slice)

--| Alias for `append`.
fn (&Slice[]) `+`(arr AsSpan[T]) -> Self:
    return Self.append[2]($, arr)

--| Appends a value to the end of the `Slice`.
fn (&Slice[]) append(val T) -> Self:
    res := $clone_on_resize($_len + 1)
    @ptr_init(res.ptr + res._len, val)
    res._len += 1
    return res

--| Appends the elements of a `Span` to the end of this `Slice`.
fn (&Slice[]) append(span [&]T) -> Self:
    if span.len() == 0:
        return $.*
    
    res := $clone_on_resize($_len + span.length)
    res.ptr[res._len..res._len + span.length].init(as span)
    res._len += span.length
    return res

--| Appends the elements of a `AsSpan` to the end of this array.
fn (&Slice[]) append(arr AsSpan[T]) -> Self:
    return $append(arr.span())

-- fn (&Slice[]) cap() -> int:
--     return $cap

fn (&Slice[]) clear() -> Self:
    return $size_down(0)

-- fn (&Slice[]) clone() -> Self:
--     new_buf := alloc(T, $length).ptr
--     new_buf[0..$length].init($buf[0..$length])
--     return {
--         buf    = new_buf,
--         length = $length,
--         cap    = $length,
--     }

fn (&Slice[]) contains(needle T) -> bool:
    return $span().contains(needle)

fn (&Slice[]) clone_on_resize(new_cap int) -> Self:
    sub_slice := ($header >> 63 != 0)
    if !sub_slice and @isUniqueRef(@unsafeCast(Ptr[void], $buf)) and new_cap <= $header:
        return $.*

    if new_cap < $_len:
        new_cap = $_len

    if !sub_slice:
        if new_cap > $header:
            new_cap = compute_new_cap($header, new_cap)
        else:
            new_cap = compute_new_cap($_len, new_cap)
    else:
        new_cap = compute_new_cap($_len, new_cap)

    -- Copy underlying buffer to new buffer.
    buf := new_slice_buffer(T, new_cap)
    elems_ptr := as[Ptr[T]] buf.elems_ptr()
    elems_ptr[0..$_len].init($ptr[0..$_len])
    return {
        buf = buf,
        ptr = elems_ptr,
        _len = $_len,
        header = new_cap,
    }

-- fn (&&Slice[]) ensureUnusedCap(unused int):
--     cap := $cap()
--     if cap - $_len < unused:
--         $ensure_cap(cap + 1)

--| Returns the first index of `needle` in the array or `none` if not found.
fn (&Slice[]) index(needle T) -> ?int:
    return $[..].index(needle)

--| Inserts a value at index `idx`.
fn (&Slice[]) insert(idx int, val T) -> Self:
    if idx.ugt($_len):
        panic_oob(idx)

    cur := $clone_on_resize($_len + 1)

    if idx == cur._len:
        -- Appending to end.
        @ptr_init(cur.ptr + cur._len, val)
        cur._len += 1
        return cur

    if idx < cur._len:
        @ptr_init(cur.ptr + cur._len, cur.ptr[cur._len-1])
    if idx + 1 < cur._len:
        cur.ptr[idx+1..cur._len].set_backwards(cur.ptr[idx..cur._len-1])
    cur.ptr[idx] = val
    cur._len += 1
    return cur

--| Returns a new iterator over the slice elements.
fn (&Slice[]) iterator() -> int:
    return 0

--| Returns a new string that joins the elements with `separator`.
fn (&Slice[]) join(sep str) -> str:
    if $_len == 0:
        return ''
    res := str($ptr[0])
    for 1..$_len |i|:
        res += sep + str($ptr[i])
    return res

fn (&Slice[]) last() -> T:
    return $[$_len-1]

fn (&Slice[]) len() -> int:
    return $_len

-- fn (sink Slice[]) as_buffer() -> Buffer[T]:
--     buf := $buf
--     length := $length
--     cap := $cap()
--     @consume($)
--     return {
--         base   = buf,
--         length = length,
--         header = cap,
--     }

fn (&Slice[]) next(idx &int) -> ?T:
    if idx.* >= $_len:
        return none

    val := $[idx.*]
    idx.* += 1
    return val

--| Removes an element at index `idx`.
fn (&Slice[]) remove(idx int) -> Self:
    cur := $clone_on_resize(0)
    @destruct(T, cur.ptr + idx)
    -- Move rest to the left.
    size := type.size(T) * (cur._len-idx-1)
    @memmove(as (cur.ptr + idx), as (cur.ptr + idx + 1), size)
    cur._len -= 1
    return cur

--| Decreases the size of the `Slice` to `new_len` elements.
--| If the new size is smaller, elements at the end of the `Slice` are removed.
fn (&Slice[]) size_down(new_len int) -> Self:
    if new_len.ugt($_len):
        panic('Invalid argument.')
    if new_len == $_len:
        return

    cur := $clone_on_resize(0)
    #if type.managed(T):
        cur.ptr[new_len..cur._len].destruct()
    cur._len = new_len
    return cur

--| Increases the size of the `Slice` to `new_len` elements. If the new size is bigger, `init` values
--| are appended to the `Slice`.
fn (&Slice[]) size_up(new_len int, init T) -> Self:
    if new_len.ult($_len):
        panic('Invalid argument.')
    if new_len == $_len:
        return

    cur := $clone_on_resize(new_len)

    -- Initialize new slots with `init` value.
    cur.ptr[cur._len..new_len].init(init)
    cur._len = new_len
    return cur

fn (&Slice[]) set_last(val T) -> void:
    $[$_len-1] = val

--| Sorts the array with the given `less` function.
--| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
fn (&Slice[]) sort(less &OpaqueFunc[LessFn[T]]):
    return $span().sort(less)

fn (scope &Slice[]) span() -> scope [&]T:
    return {
        base   = as $ptr,
        length = $_len,
    }

-- --| Returns the first index of any `bytes` in `set` or `none` if not found.
-- #[bind] fn Slice.findAny(self, set []T) -> ?int

-- --| Returns a list of arrays split at occurrences of `sep`.
-- #[bind] fn Slice.split(self, sep []T) -> List[Array]

-- --| Returns a span with ends trimmed from `delims`. `mode` can be @left, @right, or @ends.
-- #[bind] fn Slice.trim(self, mode symbol, delims []T) -> []T

type AsSpan[T Any] trait:
    fn span() -> scope [&]T

--| An implicit trait that conforms to a const eligible type.
type Const trait

fn Const :: @predicate(T type) -> bool:
    return type.is_const(T)

--| TODO: A `ConstSpan` behaves like a `Span` except it can only perform read operations.
--| The element type `T` is restricted to be a const eligible type.
type ConstSpan[T Const]

--| A `Span` is a borrowed view over a sequence of elements.
--| It can also be denoted as `[&]T` where `T` is the element type.
type Span[T Any]:
    with AsSpan[T]
    base   &T
    length int

fn Span[] :: @init_sequence(scope span [&]T) -> scope [&]T:
    return span

fn (&Span[]) @index_addr(idx int) -> Ptr[T]:
    if idx.uge($length):
        panic_oob(idx)
    return as[Ptr[T]] $base + idx

fn (scope &Span[]) @slice() -> scope [&]T:
    return $.*

fn (scope &Span[]) @slice(start int) -> scope [&]T:
    return $[start..$length]

fn (scope &Span[]) @slice(start int, end int) -> scope [&]T:
    if start > $length:
        panic_oob(start)
    if end > $length:
        panic_oob(end)
    if end < start:
        panic('InvalidArgument')
    return {
        base   = as ((as[Ptr[T]] $base) + start),
        length = end - start,
    }

fn (scope &Span[]) as_bytes() -> scope [&]byte:
    return as (as[Ptr[byte]] $base)[0..$length * type.size(T)]

fn (scope &Span[]) span() -> scope [&]T:
    return $.*

fn (&Span[]) `==`(o Self) -> bool:
    a_len := a.len()
    if a_len != b.len():
        return false
    if a_len == 0 or a.ptr == b.ptr:
        return true
    for 0..a_len |i|:
        if a[i] != b[i]:
            return false
    return true 

fn (&Span[]) contains(needle T) -> bool:
    return $index(needle) != none

fn (&Span[]) index(needle T) -> ?int:
    return (as[Ptr[T]] $base)[0..$length].index(needle)

fn (&Span[]) iterator() -> int:
    return 0

fn (&Span[]) len() -> int:
    return $length

fn (&Span[]) next(idx &int) -> ?T:
    if idx.* == $length:
        return none

    val := $[idx.*]
    idx.* += 1
    return val

fn (&Span[]) init(o Self):
    @unsafeCast(&PtrSpan[T], $).init(as[PtrSpan[T]] o)

fn (&Span[]) init(val T):
    @unsafeCast(&PtrSpan[T], $).init(val)

fn (&Span[]) set(o Self):
    @unsafeCast(&PtrSpan[T], $).set(as[PtrSpan[T]] o)

fn (&Span[]) set(val T):
    @unsafeCast(&PtrSpan[T], $).set(val)

--| Sorts the span with the given `less` function.
--| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
fn (&Span[]) sort(less &OpaqueFunc[LessFn[T]]):
    -- Simple insertion sort, will be upgraded to pdqsort later on.
    buf := as[Ptr[T]] $base
    for 1..$length |i|:
        cur := buf[i]
        ii := i-1
        while ii >= 0:
            if less.*(cur, buf[ii]):
                buf[ii+1] = buf[ii]
                ii -= 1
            else: break
        buf[ii + 1] = cur

--| Returns whether the `Span` ends with `target`.
fn (&Span[]) ends_with(target Self) -> bool:
    if $len() < target.len():
        return false
    for $[$len()-target.len()..] |i, elem|:
        if elem != target[i]:
            return false
    return true

--| Returns whether the `Span` starts with `target`.
fn (&Span[]) starts_with(target Self) -> bool:
    if $len() < target.len():
        return false
    for $[0..target.len()] |i, elem|:
        if elem != target[i]:
            return false
    return true

--| A dynamically sized data structure that contains a sequence of elements.
--| Meant for storing and retrieving small value types such as primitives and references.
--| Cannot be sliced and does not allow referencing to the elements.
--| These constraints allow resizing operations without an exclusive borrow.
type ArrayStore[T Any]:
    with NoCopy
    buf    Ptr[T]
    length int
    cap    int

fn ArrayStore[] :: @init_sequence(init [&]T) -> Self:
    if init.length == 0:
        return {
            buf    = none,
            length = 0,
            cap    = 0,
        }
    cap := compute_new_cap(0, init.length)
    buf := alloc(T, cap).ptr
    buf[0..init.length].init(as init)
    return {
        buf    = buf,
        length = init.length,
        cap    = cap,
    }

fn (&ArrayStore[]) @deinit():
    #if type.managed(T):
        for 0..$length |i|:
            @destruct(T, $buf + i)
    free($buf)

fn (&ArrayStore[]) @index(idx int) -> T:
    if idx.uge($length):
        panic_oob(idx)
    return $buf[idx]

fn (&ArrayStore[]) @set_index(idx int, val T):
    if idx.uge($length):
        panic_oob(idx)
    $buf[idx] = val

--| Alias for `append`.
fn (&ArrayStore[]) `<<`(val T):
    Self.append[0]($, val)

--| Alias for `append`.
fn (&ArrayStore[]) `<<`(arr AsSpan[T]):
    Self.append[2]($, arr.span())

--| Appends a value to the end of the array.
fn (&ArrayStore[]) append(val T):
    $ensure_cap($length + 1)
    @ptr_init($buf + $length, val)
    $length += 1

--| Appends the elements of a `Span` to the end of this `ArrayStore`.
fn (&ArrayStore[]) append(slice [&]T):
    if slice.len() == 0:
        return
    $ensure_cap($length + slice.length)
    $buf[$length..$length + slice.length].init(as slice)
    $length += slice.length

--| Appends the elements of a `AsSpan` to the end of this `ArrayStore`.
fn (&ArrayStore[]) append(arr AsSpan[T]):
    $append(arr.span())

fn (&ArrayStore[]) ensure_cap(new_cap int):
    if $cap >= new_cap:
        return

    final_cap := compute_new_cap($cap, new_cap)
    if $buf != none:
        new_buf := alloc(T, final_cap).ptr

        -- TODO: Move old elements to new buffer.
        new_buf[0..$length].init($buf[0..$length])
        #if type.managed(T):
            for 0..$length |i|:
                @destruct(T, $buf + i)
        free($buf)

        $buf = new_buf
        $cap = final_cap
    else:
        new_buf := alloc(T, final_cap).ptr
        $buf = new_buf
        $cap = final_cap

fn (&ArrayStore[]) len() -> int:
    return $length

--| A dynamically sized data structure that contains a sequence of elements.
--| Can be cloned or sliced into mutable spans.
--| Any operation that resizes the array requires an exclusive borrow.
type Array[T Any]:
    with NoCopy
    with AsSpan[T]
    buf    Ptr[T]
    length int

    -- 2-bit MSB is reserved for `Ex` borrowing.
    -- The rest represents the capacity.
    header int

fn (&Array[]) @deinit():
    #if type.managed(T):
        for 0..$length |i|:
            @destruct(T, $buf + i)
    free($buf)

fn Array[] :: @init_sequence(init [&]T) -> Self:
    if init.length == 0:
        return {
            buf    = none,
            length = 0,
            header = 0,
        }
    cap := compute_new_cap(0, init.length)
    buf := alloc(T, cap).ptr
    buf[0..init.length].init(as init)
    return {
        buf    = buf,
        length = init.length,
        header = cap,
    }

--| Creates an array with initial length and capacity of `n` and values set to `val`.
fn Array[] :: @init(n int, val T) -> Self:
    buf := alloc(T, n).ptr
    buf[0..n].init(val)
    return {
        buf    = buf,
        length = n,
        header = n,
    }

fn (&Array[]) @acquire_ex_borrow() -> bool:
    if ($header && (1 << 63)) == 0:
        $header ||= 1 << 63
        return true
    return false

fn (&Array[]) own_msb() -> int:
    return $header && (3 << 62)

fn (&Array[]) @release_ex_borrow():
    $header &&= ~(3 << 62)

fn (&Array[]) @acquire_borrow() -> AcquireBorrowKind:
    if ($header && (1 << 63)) != 0:
        return @ex_borrowed
    if ($header && (1 << 62)) == 0:
        $header ||= 1 << 62
        return @acquired
    return @borrowed

fn (&Array[]) @release_borrow():
    $header &&= ~(3 << 62)

fn (scope &Array[]) @index_addr(idx int) -> scope &T:
    if idx.uge($length):
        panic_oob(idx)
    return @scope_ptr_to_borrow('self', as[Ptr[T]] $buf + idx)

fn (&Array[]) @index(idx int) -> T:
    if idx.uge($length):
        panic_oob(idx)
    return $buf[idx]

fn (&Array[]) @set_index(idx int, val T):
    if idx.uge($length):
        panic_oob(idx)
    $buf[idx] = val

fn (scope &Array[]) span() -> scope [&]T:
    return $[..]

fn (scope &Array[]) @slice() -> scope [&]T:
    return {
        base   = as $buf,
        length = $length,
    }

fn (scope &Array[]) @slice(start int) -> scope [&]T:
    if start > $length:
        panic_oob(start)
    return {
        base   = as ($buf + start),
        length = $length - start,
    }

fn (scope &Array[]) @slice(start int, end int) -> scope [&]T:
    if start > $length:
        panic_oob(start)
    if end > $length:
        panic_oob(end)
    if end < start:
        panic('InvalidArgument')
    return {
        base   = as ($buf + start),
        length = end - start,
    }

--| Alias for `append`.
fn (&&Array[]) `<<`(val T):
    Self.append[0]($, val)

--| Alias for `append`.
fn (&&Array[]) `<<`(span [&]T):
    Self.append[1]($, span)

--| Alias for `append`.
fn (&&Array[]) `<<`(arr AsSpan[T]):
    Self.append[2]($, move arr)

--| Appends a value to the end of the array.
fn (&&Array[]) append(val T):
    $ensure_cap($length + 1)
    @ptr_init($buf + $length, val)
    $length += 1

--| Appends the elements of a span to the end of this array.
fn (&&Array[]) append(span [&]T):
    if span.len() == 0:
        return
    $ensure_cap($length + span.length)
    $buf[$length..$length + span.length].init(as span)
    $length += span.length

--| Appends the elements of a `AsSpan` to the end of this array.
fn (&&Array[]) append(arr AsSpan[T]):
    $append(arr[..])

fn (&Array[]) cap() -> int:
    return $header && ~(3 << 62)

fn (&&Array[]) clear():
    $size_down(0)

fn (&Array[]) clone() -> Self:
    new_buf := alloc(T, $length).ptr
    new_buf[0..$length].init($buf[0..$length])
    return {
        buf    = new_buf,
        length = $length,
        cap    = $length,
    }

fn (&Array[]) contains(needle T) -> bool:
    return $[..].contains(needle)

-- NOTE: This used to be used for copy-on-resize.
-- fn (&Array[]) copyIfShared():
--     if $buf == none:
--         return
--     if $isUniqueRef(@unsafeCast(Ptr[void], $buf)):
--         return

--     -- Perform copy.
--     -- Copy shared buffer to new buffer.
--     new_buf := PartialBuffer[T].new($length)
--     new_ptr := new_buf.elemsPtr()
--     new_ptr[0..$length].init($ptr[0..$length])
--     #if type.managed(T):
--         for 0..$length |i|:
--             @destruct(T, $ptr + i)
--     $buf = new_buf
--     $buf.?.length = $length
--     $ptr = new_ptr

fn (&&Array[]) ensure_cap(new_cap int):
    cap := $cap()
    if cap >= new_cap:
        return

    final_cap := compute_new_cap(cap, new_cap)
    if $buf != none:
        new_buf := alloc(T, final_cap).ptr

        -- TODO: Move old elements to new buffer.
        new_buf[0..$length].init($buf[0..$length])
        #if type.managed(T):
            for 0..$length |i|:
                @destruct(T, $buf + i)
        free($buf)

        $buf = new_buf
        $header = $own_msb() || final_cap
    else:
        new_buf := alloc(T, final_cap).ptr
        $buf = new_buf
        $header = $own_msb() || final_cap

fn (&&Array[]) ensureUnusedCap(unused int):
    cap := $cap()
    if cap - $length < unused:
        $ensure_cap(cap + 1)

--| Returns the first index of `needle` in the array or `none` if not found.
fn (&Array[]) index(needle T) -> ?int:
    return $[..].index(needle)

--| Inserts a value at index `idx`.
fn (&&Array[]) insert(idx int, val T):
    if idx.ugt($length):
        panic_oob(idx)

    $ensure_cap($length + 1)

    if idx == $length:
        -- Appending to end.
        @ptr_init($buf + $length, val)
        $length += 1
        return

    if idx < $length:
        @ptr_init($buf + $length, $buf[$length-1])
    if idx + 1 < $length:
        $buf[idx+1..$length].set_backwards($buf[idx..$length-1])
    $buf[idx] = val
    $length += 1

--| Returns a new iterator over the slice elements.
fn (&Array[]) iterator() -> int:
    return 0

--| Returns a new string that joins the elements with `separator`.
fn (&Array[]) join(sep str) -> str:
    if $length == 0:
        return ''
    res := str($buf[0])
    for 1..$length |i|:
        res += sep + str($buf[i])
    return res

fn (&Array[]) last() -> T:
    return $[$length-1]

fn (&Array[]) len() -> int:
    return $length

fn (sink Array[]) as_buffer() -> Buffer[T]:
    buf := $buf
    length := $length
    cap := $cap()
    @consume($)
    return {
        base   = buf,
        length = length,
        header = cap,
    }

fn (&Array[]) next(idx &int) -> ?T:
    if idx.* == $length:
        return none

    val := $[idx.*]
    idx.* += 1
    return val

--| Removes an element at index `idx`.
fn (&&Array[]) remove(idx int):
    @destruct(T, $buf + idx)
    -- Move rest to the left.
    size := type.size(T) * ($length-idx-1)
    @memmove(as ($buf + idx), as ($buf + idx + 1), size)
    $length -= 1

--| Decreases the size of the array to `new_len` elements.
--| If the new size is smaller, elements at the end of the array are removed.
fn (&&Array[]) size_down(new_len int):
    if new_len.ugt($length):
        panic('Invalid argument.')
    if new_len == $length:
        return

    #if type.managed(T):
        for new_len..$length |i|:
            @destruct(T, $buf + i)
    $length = new_len

--| Increases the size of the array to `new_len` elements. If the new size is bigger, `init` values
--| are appended to the array.
fn (&&Array[]) size_up(new_len int, init T):
    if new_len.ult($length):
        panic('Invalid argument.')
    if new_len == $length:
        return

    $ensure_cap(new_len)

    -- Initialize new slots with zero value.
    $buf[$length..new_len].init(init)
    $length = new_len

fn (&Array[]) setLast(val T) -> void:
    $[$length-1] = val

--| Sorts the array with the given `less` function.
--| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
fn (&Array[]) sort(less &OpaqueFunc[LessFn[T]]):
    return $[0..].sort(less)

-- --| Returns the first index of any `bytes` in `set` or `none` if not found.
-- #[bind] fn Array.findAny(self, set []T) -> ?int

-- --| Returns a list of arrays split at occurrences of `sep`.
-- #[bind] fn Array.split(self, sep []T) -> List[Array]

-- --| Returns a span with ends trimmed from `delims`. `mode` can be @left, @right, or @ends.
-- #[bind] fn Array.trim(self, mode symbol, delims []T) -> []T

-fn compute_new_cap(cur int, min int) -> int:
    new := cur
    while:
        new += new / 2 + 8
        if new >= min:
            return new

type CompareIndexFn[Context Any] = fn(cx Context, idx int) -> CompareOrder

type CompareOrder enum:
    case eq
    case lt
    case gt

#[bind]
type FuncSig _

#[bind]
fn (FuncSig) num_params() -> int

#[bind]
fn (FuncSig) param_at(i int) -> meta.FuncParam

#[bind]
fn (FuncSig) ret() -> type

type FnTuple[const Sig FuncSig] const:
    fields := [Sig.num_params()]meta.StructField({name='', type=void, offset=0, state_offset=0})
    for 0..Sig.num_params() |i|:
        fields[i] = {
            name = 'p' + EvalStr(i),
            type = Sig.param_at(i).type,
            offset = 0,
            state_offset = 0,
        }
    return meta.new_struct(fields, {tuple=true})

--| Function pointer.
#[bind] type funcptr_t[const SIG FuncSig] _

--| Function union. Requires a `funcptr_t` parameter.
--| Can contain static functions or closures.
#[bind]
type Func[SIG Any] _

#[bind] fn (Func[]) @size() -> int

--| Opaque function union. Cannot be moved or copied.
--| These constraints allow it to contain a pinned closure which can capture locals as well as references.
--| A `Func` can be implicitly converted to an `OpaqueFunc`.
#[bind] type OpaqueFunc[SIG Any] _

#[bind] fn (OpaqueFunc[]) @size() -> int

-- Function symbol.
#[bind] type funcsym_t[SIG FuncSig] _

type LessFn[T Any] = fn(T, T) -> bool

#[bind]
type Table:
    inner ^void

-- #[bind]
-- type Table ^MapValue.Auto[str, Object]

-- fn Table.@init_record(pairs InitSpan[Pair[str, Object]]) -> Table:
--     new := ^MapValue.Auto[str, Object].$init_record(pairs)
--     return new as Self

-- fn Table.@get(self, name str) -> Object:
--     return (self as ^MapValue.Auto[str, Object])[name]

-- fn Table.@set(self, name str, value Object):
--     (self as ^MapValue.Auto[str, Object])[name] = value

type Pair[K Any, V Any]:
    left  K
    right V

type RefChild[P Any, T Any]:
    parent ^P
    ptr    Ptr[T]

type HashFn[T Any] = fnsym(T) -> int
type EqFn[T Any] = fnsym(T, T) -> bool

--| NOTE: Unused, may be redesigned or removed.
--| A precomputed hash map from an initial set of entries.
--| Insertions and deletions are not allowed.
--| Implemented with open addressing and linear probing.
type StaticMap[K type, V type, HASH HashFn[K], EQ EqFn[K]]:
    vacs Ptr[bool]
    keys Ptr[K]
    vals Ptr[V]
    len  int
    cap  int

fn StaticMap[] :: init(pairs []Pair[K, V]) -> Self:
    nslots := pairs.len() * 100 / HashMap.LoadFactor
    return init(nslots, pairs)

fn StaticMap[] :: init(nslots int, pairs []Pair[K, V]) -> Self:
    -- TODO: Alloc one contiguous array.
    new := Self{
        vacs = alloc(bool, nslots),
        keys = alloc(K, nslots),
        vals = alloc(V, nslots),
        len  = 0,
        cap  = nslots,
    }
    new.vacs[0..nslots].set(true)

    for pairs |pair|:
        new.set(pair.left, pair.right)

    return new

fn (&StaticMap[]) is_vacant(slot int) -> bool:
    return @vacs[slot]

fn (&StaticMap[]) set(key K, val V):
    final_slot := $cap

    mask := $cap - 1
    hash := HASH(key)
    slot := hash && mask

    empty := @is_vacant(slot)
    if empty:
        final_slot = slot
        @keys[final_slot] = key
        @vacs[final_slot] = false
        $len += 1
    else:
        first := slot
        while:
            if empty:
                final_slot = slot
                @keys[final_slot] = key
                @vacs[final_slot] = false
                $len += 1
                break

            if EQ(@keys[slot], key):
                final_slot = slot
                break

            -- Linear probing.
            slot = (slot + 1) && mask
            if slot == first:
                break

            empty = @isVacant(slot)

    -- No slot found.
    if final_slot == $cap:
        panic('NoSpace')

    $vals[final_slot] = val

fn (&StaticMap[]) get(key K) -> ?Ptr[V]:
    final_slot := $cap

    mask := $cap - 1
    hash := HASH(key)
    slot := hash && mask

    empty := @isVacant(slot)
    if empty:
        return none

    first := slot
    while:
        if empty:
            return none
        if EQ(@keys[slot], key):
            return *$vals[slot]

        -- Linear probing.
        slot = (slot + 1) && mask
        if slot == first:
            return none

        empty = @isVacant(slot)

fn (&StaticMap[]) @index(key K) -> Ptr[V]:
    if @get(key) |val|:
        return val
    else:
        panic('MissingKey')

-- fn (&StaticMap[]) @deinit(mem Memory) void:
--     free($vacs[0..$cap])
--     free($keys[0..$cap])
--     free($vals[0..$cap])

type StaticMap :: Auto[K Any, V Any] = StaticMap[K, V, AutoHash[K], AutoEq[K]]

type Map[K Any, V Any]:
    base HashMap[K, V, AutoHash[K], AutoEq[K]]

with [K Copyable, V Copyable]
fn Map[] :: @init_record(pairs [&]Pair[K, V]) -> Self:
    new := HashMap[K, V, AutoHash[K], AutoEq[K]].@init_record(pairs)
    return meta.init_type(Self, {base=move new}) 

fn (&Map[]) @index(key K) -> V:
    return $base.@index(key)

fn (&Map[]) @set_index(key K, val V):
    $base.@set_index(key, val)

fn (&Map[]) clear():
    $base.clear()

fn (&Map[]) get(key K) -> ?V:
    return $base.get(key)

fn (&Map[]) iterator() -> int:
    return $base.iterator()

fn (&Map[]) keys() -> []K:
    return $base.keys() 

fn (&Map[]) next(idx &int) -> ?MapEntry[K, V]:
    return $base.next(idx)

fn (&Map[]) set(key K, val V):
    $base.set(key, val)

fn (&Map[]) size() -> int:
    return $base.size()

fn (&Map[]) try_remove(key K) -> bool:
    return $base.try_remove(key)

fn (&Map[]) remove(key K):
    $base.remove(key)

fn (&Map[]) contains(key K) -> bool:
    return $base.contains(key)

--| Generic hash map implemented with open addressing and linear probing.
type HashMap[K Any, V Any, const HASH HashFn[K], const EQ EqFn[K]]:
    with NoCopy
    -- Metadata currently indicates 0 (vacant), 1 (value), 2 (tombstone)
    -- TODO: Mimic swiss tables by storing 7-bit hash and flag bit for SIMD lookup.
    -- TODO: Store all arrays in one array.
    meta  Ptr[byte]
    _keys Ptr[K]
    vals  Ptr[V]

    --| The number of key-value pairs in the map.
    len  int

    -- Number of vacant slots.
    nvac int
    cap  int

const HashMap :: LoadFactor = 75

with [K Copyable, V Copyable]
fn HashMap[] :: @init_record(pairs [&]Pair[K, V]) -> Self:
    new := meta.init_type(Self, {
        meta = none,
        _keys = none,
        vals = none,
        nvac = 0,
        len  = 0,
        cap  = 0,
    })
    for 0..pairs.length |i|:
        pair := &pairs[i]
        new.set(pair.left, pair.right)
    return move new

fn (&HashMap[]) @index(key K) -> V:
    slot := $findSlot(key)
    if slot == -1:
        panic('MissingKey')
    return $vals[slot]

fn (&HashMap[]) @set_index(key K, val V):
    $set(key, val)

fn (&HashMap[]) clear():
    for 0..$cap |i|:
        if $meta[i] == 1:
            #if type.managed(K):
                @destruct(K, *$_keys[i])
            #if type.managed(V):
                @destruct(V, *$vals[i])
            $meta[i] = 0
    $len = 0
    $nvac = $cap

--| Returns whether there is a value mapped to `key`.
fn (&HashMap[]) contains(key K) -> bool:
    return $findSlot(key) != -1

--| Returns value mapped to `key` or returns `none`.
fn (&HashMap[]) get(key K) -> ?V:
    slot := $findSlot(key)
    if slot == -1:
        return none
    return $vals[slot]

fn (&HashMap[]) grow():
    if $cap == 0:
        $resize(2 ** 3)
    else:
        $resize($cap << 1)

--| Assumes there is at least 1 vacant slot.
--| If the entry already exists, the sign bit is set on the result.
--| This returns the first tombstone slot if the key is missing unlike `findSlot`.
-fn (&HashMap[]) findSetSlot(key K) -> int:
    mask := $cap - 1
    hash := HASH(key)
    slot := hash && mask
    slot_info := $meta[slot]
    if slot_info == 0:
        return slot

    first_tomb := -1
    if slot_info == 1:
        if EQ($_keys[slot], key):
            return slot || int.min
    else:
        first_tomb = slot

    -- Perform linear probing.
    while:
        slot = (slot + 1) && mask
        slot_info = $meta[slot]
        if slot_info == 0:
            if first_tomb != -1:
                return first_tomb
            return slot

        if slot_info == 1:
            if EQ($_keys[slot], key):
                return slot || int.min

--| Returns -1 if not found.
fn (&HashMap[]) findSlot(key K) -> int:
    if $cap == 0:
        return -1
    mask := $cap - 1
    hash := HASH(key)
    slot := hash && mask
    slot_info := $meta[slot]

    if slot_info == 0:
        return -1

    if slot_info == 1:
        if EQ($_keys[slot], key):
            return slot
            
    -- Perform linear probing.
    first := slot
    while:
        slot = (slot + 1) && mask
        if slot == first:
            return -1

        slot_info = $meta[slot]
        if slot_info == 0:
            return -1

        if slot_info == 1:
            if EQ($_keys[slot], key):
                return slot

--| Iterates over the map elements.
fn (&HashMap[]) iterator() -> int:
    return 0

fn (&HashMap[]) keys() -> []K:
    res := []K{}
    iter := @iterator()
    while @next(&iter) |e|:
        res << e.key
    return res

fn (&HashMap[]) next(idx &int) -> ?MapEntry[K, V]:
    while idx.* != $cap:
        if $meta[idx.*] != 1:
            idx.* += 1
            continue

        res := MapEntry[K, V]{
            key   = $_keys[idx.*],
            value = $vals[idx.*],
        }
        idx.* += 1
        return res

    return none

--| Removes the element with the given key `key`.
fn (&HashMap[]) try_remove(key K) -> bool:
    slot := $findSlot(key)
    if slot == -1:
        return false

    mask := $cap - 1
    next := (slot + 1) && mask
    if $meta[next] == 0:
        -- Adjacent slot is empty, reset to empty.
        $meta[slot] = 0
        $nvac += 1
    else:
        $meta[slot] = 2

    @destruct(K, *$_keys[slot])
    @destruct(V, *$vals[slot])

    $len -= 1
    return true

--| Removes the element with the given key `key` or panic.
fn (&HashMap[]) remove(key K):
    if !$try_remove(key):
        panic('Missing key %{key}')

--| Perform rehashing.
fn (&HashMap[]) resize(new_cap int):
    old_meta := $meta
    old_keys := $_keys
    old_vals := $vals
    old_cap  := $cap

    $meta = alloc(byte, new_cap).ptr
    $meta[0..new_cap].set(0)
    $_keys = alloc(K, new_cap).ptr

    $vals = alloc(V, new_cap).ptr
    $len = 0
    $nvac = new_cap * HashMap.LoadFactor / 100
    $cap = new_cap

    if old_cap == 0:
        return

    for old_meta[0..old_cap] |i, slot_data|:
        if slot_data != 1:
            continue
        $set(old_keys[i], old_vals[i])

    for 0..old_cap |i|:
        if old_meta[i] == 1:
            #if type.managed(K):
                @destruct(K, *old_keys[i])
            #if type.managed(V):
                @destruct(V, *old_vals[i])
    free(old_meta)
    free(old_keys)
    free(old_vals)

fn (&HashMap[]) set(key K, val V):
    if $nvac == 0:
        $grow()

    slot := $findSetSlot(key)
    if slot && int.min == 0:
        -- Initialize.
        @ptr_init($_keys + slot, key)
        $meta[slot] = 1
        @ptr_init($vals + slot, val)
    else:
        -- Replace.
        $vals[slot && int.max] = val

    $nvac -= 1
    $len += 1

fn (&HashMap[]) size() -> int:
    return $len

fn (&HashMap[]) @deinit():
    if $cap == 0:
        return

    for 0..$cap |i|:
        if $meta[i] == 1:
            #if type.managed(K):
                @destruct(K, *$_keys[i])
            #if type.managed(V):
                @destruct(V, *$vals[i])

    free($meta)
    free($_keys)
    free($vals)

fn @memeq(a []%T, b []T) -> bool:
    a_len := a.len()
    if a_len != b.len():
        return false

    if a_len == 0 or a.ptr == b.ptr:
        return true

    -- TODO: Compare fixed array of bytes at a time if the element type does not have padding or unions.
    a_ptr := as[Ptr[byte]] a.ptr
    b_ptr := as[Ptr[byte]] b.ptr
    for 0..a_len * type.size(T) |i|:
        if a_ptr[i] != b_ptr[i]:
            return false
    return true

--| Assumes no-alias `dst` and `src` regions.
#[bind] fn @memcpy(dst Ptr[byte], src Ptr[byte], len int)

#[bind] fn @memset(dst Ptr[byte], val Ptr[byte], val_len int, n int)

#[bind] fn @memmove(dst Ptr[byte], src Ptr[byte], len int)

#[bind]
fn @new_object_undef(type_id int, nbytes int) -> Ptr[void]

#[bind='@new_object_undef2']
fn @new_object_undef(thread Thread, type_id int, nbytes int) -> Ptr[void]

fn @init_astr_undef(len int) -> str:
    buf := as[^StrBuffer] @new_object_undef(type.id(StrBuffer), 8 + len)
    buf.inner.length = len
    return {
        buf    = buf,
        ptr    = as *buf.inner.data,
        header = len || (1 << 63),
    }

fn @init_astr_undef(thread Thread, len int) -> str:
    buf := as[Ptr[StrBuffer]] @new_object_undef(thread, type.id(StrBuffer), 8 + len)
    buf.inner.length = len
    var ptr Ptr[byte] = as *buf.inner.data
    return {
        buf    = as buf,
        ptr    = ptr,
        header = len || (1 << 63),
    }

fn @init_ustr_undef(len int) -> str:
    buf := as[^StrBuffer] @new_object_undef(type.id(StrBuffer), 8 + len)
    buf.inner.length = len
    return {
        buf    = buf,
        ptr    = as *buf.inner.data,
        header = len,
    }

fn @init_ustr_undef(thread Thread, len int) -> str:
    buf := as[^StrBuffer] @new_object_undef(thread, type.id(StrBuffer), 8 + len)
    buf.inner.length = len
    return {
        buf    = buf,
        ptr    = as *buf.inner.data,
        header = len,
    }

#[bind] fn @releaseOnly(obj Ptr[void]) -> bool

#[bind] fn @retain(obj Ptr[void])

--| Returns the current reference count of an object.
#[bind] fn @refCount(obj Object) -> int

#[bind] fn @isUniqueRef(ptr Ptr[void]) -> bool

--| Insert a nop IR with a label.
#[bind] fn @nop(label EvalStr)

#[bind] fn @breakpoint()

type DeinitFn = fn(Ptr[void]) -> void

type ClosureData:
    padding     int
    numCaptured int
    captured    void

--| Consumes a local. Does not invoke destructor.
#[bind] fn @consume(a Code)

--| Compare like a primitive.
#[bind] fn @cmp(a Code, b Code) -> bool

fn @memByteEqs(a PtrSpan[byte], b PtrSpan[byte]) -> bool:
    if a.length != b.length: return false
    if a.length == 0 or a.ptr == b.ptr: return true
    for 0..a.len() |i|:
        if a[i] != b[i]:
            return false
    return true

fn @memEqs(a PtrSpan[%T], b PtrSpan[T]) -> bool:
    a_bytes := PtrSpan{
        ptr = a.ptr,
        len = a.length * type.size(T),
    }
    b_bytes := PtrSpan{
        ptr = b.ptr,
        len = b.length * type.size(T),
    }
    return @memByteEqs(a_bytes, b_bytes)

--| This is invoked for the comparison operator for non primitive types.
--| Comparing references returns true if they refer to the same instance.
fn @eq(a &%T, b &T) -> bool:
    #switch type.info(T):
        #case .struct |struct_t|:
            #if T == str:
                -- TODO: Consider moving this overloaded operator for `str`.
                return @memByteEqs(a.ptr[0..a.len()], b.ptr[0..b.len()])
            #for 0..struct_t.fields.len() |i|:
                #field := struct_t.fields[i]
                if !@eq(&meta.access(a, field.name), &meta.access(b, field.name)):
                    return false
            return true
            -- #else:
            --     meta.error('Unsupported: ' + type.name(T))

        #case .option |option_t|:
            a_none := meta.is_none(a)
            b_none := meta.is_none(b)
            if a_none and b_none:
                return true
            if a_none or b_none:
                return false
            return meta.access_option_payload(a.*) == meta.access_option_payload(b.*)

        #case .result |result_t|:
            a_err := meta.is_result_error(a)
            b_err := meta.is_result_error(b)
            if a_err and b_err:
                return meta.access_result_error(a.*) == meta.access_result_error(b.*)
            if a_err or b_err:
                return false
            return meta.access_result_payload(a.*) == meta.access_result_payload(b.*)

        #case .int:
            return a.* == b.*

        #case .float:
            return a.* == b.*

        #case .ptr:
            return a.* == b.*

        #else:
            meta.error('Unsupported: ' + type.name(T))

--| Marked unsafe to allow copying borrow containers.
#[bind, unsafe]
fn @copyStruct(%T type, src_ptr Code) -> T

--| Casting without compatibility checking.
#[bind, unsafe] fn @unsafeCast(%T type, src Code) -> T

#[bind, unsafe]
fn @scope_ptr_to_borrow(scope_param EvalStr, ptr Ptr[%T]) -> &T

#[bind] fn @bitCast(%T type, x %S) -> T

type TraitStruct:
    -- This particular field placement works well with the bc call convention.
    vtable r64  -- Ptr[void]
    impl   r64  -- Ptr[void]

fn @send(dst Thread, src Ptr[%T]) -> T:
    @nop('@send: ' + type.name(T))

    #switch type.info(T):
        #case .struct:
            #if meta.has_decl(T, '@send'):
                return src.@send(dst)
            #else:
                meta.error('Expected sendable: ' + type.name(T))
                -- return @copyStruct(T, src)
        #case .int:
            return src.*
        #else:
            meta.error('Expected sendable: ' + type.name(T))

--| Generic copy for any type.
--| Marked unsafe to allow copying borrow containers.
#[unsafe]
fn @copy(src Ptr[%T]) -> T:
    @nop('@copy: ' + type.name(T))
    #switch type.info(T):
        #case .struct |struct_t|:
            #if meta.has_decl(T, '@copy'):
                return src.@copy()
            -- TODO: @copyStruct should be implemented as:
            -- #for fields -> i, field:
            --     -- Tracked so that any panic can destruct critical values.
            --     (field.name) := meta.access(val, field.name)
            -- new T = undefined
            -- #for fields -> i, field:
            --     $setInit(meta.access(new, field.name), move $ident(field.name))
            -- return move new
            #else:
                return @copyStruct(T, src)

        #case .cstruct |cstruct_t|:
            return @copyStruct(T, src)

        #case .vector |vector_t|:
            -- TODO: Vector.@copy should be implemented as:
            -- -- PartialVector so that a critical destruction knows which elements are initialized.
            -- new PartialVector[vector_t.n, T] = undefined
            -- for 0..vector_t.n -> i:
            --     new[i] = src[i]
            --     new.len = i + 1
            -- return move new.arr

            -- Cast to Vector in case it's a distinct type.
            res := @unsafeCast(Ptr[[vector_t.len]vector_t.elem], src).@copy()
            return @bitCast(T, move res)

        #case .option |option_t|:
            if meta.is_none(src):
                return none
            else:
                payload := *meta.access_option_payload(src.*)
                return payload.*

        #case .result |result_t|:
            if meta.is_result_error(src):
                return meta.access_result_error(src)
            else:
                payload := *meta.access_result_payload(src.*)
                return payload.*

        #case .choice |choice_t|:
            switch meta.choice_tag(src.*):
                #for meta.enum_values(T.Tag) |Tag|:
                    case Tag:
                        payload := *meta.access_choice_case(src.*, Tag)
                        return meta.init_choice(T, Tag, payload.*)

        #case .ref_trait:
            trait_s := as[Ptr[TraitStruct]] src
            return @unsafeCast(T, @copyStruct(TraitStruct, trait_s))

        #case .borrow_trait:
            trait_s := as[Ptr[TraitStruct]] src
            return @unsafeCast(T, @copyStruct(TraitStruct, trait_s))

        #case .ref |ref_t|:
            return src.*

        #case .bool:
            return src.*

        #case .float:
            return src.*

        #case .int:
            return src.*

        #case .ptr:
            return src.*

        #case .void:
            return _

        #case .func |func_t|:
            #switch func_t.kind:
                #case .ptr:
                    return src.*
                #case .union:
                    return src.*
                #else:
                    panic('Unsupported')

        #else:
            panic('Unsupported: ' + type.name(T))

fn @enumToInt(val %T) -> int:
    return as val

fn @intToEnum(%T type, val int) -> T:
    return @intToEnumResult(T, val) !else:
        panic('Cannot convert `%{val}` to the enum `%{type.name(T)}`')

fn @intToEnumResult(%T type, val int) -> !T:
    switch val:
        #for meta.enum_int_values(T) |TagValue|:
            case TagValue:
                return @unsafeCast(T, val)
        else:
            return error.InvalidArgument

--| Returns a function to release and destroy for a object runtime type.
--| Returns `none` if the function is a no-op.
--| Panics if the function could not be found.
#[bind] fn @getDeinitObject(obj Ptr[void]) -> ?DeinitFn

--| Returns `@destruct` for the given type.
#[bind] fn @getTypeDtor(type_id int) -> ?DeinitFn

-- Frees the object given its byte size.
#[bind] fn @freeObject(ref Ptr[void], size int)

-- Exposed for VM. Assumes `obj` has rc=0.
fn @deinitObject[T Any](obj Ptr[void]):
    @nop('destruct object: ' + type.name(T))

    #switch type.info(T):
        #case .type:
            @freeObject(obj, 8)
        #case .func |func_t|:
            #switch func_t.kind:
                #case .ptr:
                    @freeObject(obj, 8)
                #case .union:
                    if meta.get_closure_data(obj) |data|:
                        captured := as[Ptr[r64]] *data.captured
                        for 0..data.numCaptured |i|:
                            captured_ptr := (as[Ptr[Ptr[void]]] *captured[i]).*
                            if @releaseOnly(captured_ptr):
                                if @getDeinitObject(captured_ptr) |deinit|:
                                    deinit(captured_ptr)
                        @freeObject(obj, 24 + data.numCaptured * 8)
                    else:
                        @freeObject(obj, 24)
                #case .stack_union:
                    @freeObject(obj, @unsafeCast(Ptr[T], obj).@size())
                #else:
                    panic('Unsupported')
        #else:
            @destruct(T, as obj)
            #if meta.has_decl(T, '@size'):
                @freeObject(obj, @unsafeCast(Ptr[T], obj).@size())
            #else:
                @freeObject(obj, as type.size(T))

--| Exposed for VM.
fn @destructBox(ptr Ptr[void]):
    if @getDeinitObject(ptr) |deinit|:
        deinit(ptr)

--| Partial destructor.
fn @partial_destruct(%T type, %Layout PartialStructLayout, val Ptr[T]):
    @nop('partial destruct: ' + type.name(T))

    #if meta.has_decl(T, '@deinit'):
        meta.error('Cannot invoke partial destructor with custom `' + type.name(T) + '.@deinit`')

    #switch type.info(T):
        #case .struct |struct_t|:
            #for 0..struct_t.fields.len() |i|:
                #field := struct_t.fields[i]
                #switch type.info(field.type):
                    #case .struct:
                        -- TODO: Skip when all sub fields are inactive.
                        #if Layout.field_layout(field.state_offset, type.struct_state_len(field.type)) |layout|:
                            -- Field contains a partial type layout.
                            #partial_destruct(field.type, layout, *meta.access(val, field.name))
                        #else:
                            -- Regular active field.
                            @destruct(field.type, *meta.access(val, field.name))
                    #else:
                        #if Layout.is_field_active(field.state_offset):
                            -- Regular active field.
                            @destruct(field.type, *meta.access(val, field.name))

        #else:
            panic('Unsupported partial destructor for type: ' + type.name(T))

fn @destruct_span(span PtrSpan[%T]):
    #if type.managed(T):
        for 0..span.length |i|:
            @destruct(T, *span.ptr[i])

--| Generic destructor for any type.
fn @destruct(%T type, val Ptr[T]):
    @nop('destruct: ' + type.name(T))

    -- @deinit is invoked before any child destructors.
    -- This lets the parent read from the children when they are still alive.
    #if meta.has_decl(T, '@deinit'):
        val.@deinit()

    #switch type.info(T):
        #case .struct |struct_t|:
            #for 0..struct_t.fields.len() |i|:
                #field := struct_t.fields[i]
                @destruct(field.type, *meta.access(val, field.name))

        #case .cstruct |cstruct_t|:
            -- No destructor.
            pass

        #case .cunion:
            -- No destructor.
            pass

        #case .option |option_t|:
            if !meta.is_none(val):
                @destruct(option_t.child, *meta.access_option_payload(val.*))

        #case .result |result_t|:
            if !meta.is_result_error(val):
                @destruct(result_t.child, *meta.access_result_payload(val.*))

        #case .choice |choice_t|:
            switch meta.choice_tag(val.*):
                #for meta.enum_values(T.Tag) |Tag|:
                    case Tag:
                        payload := *meta.access_choice_case(val.*, Tag)
                        @destruct(meta.CasePayload[T, Tag], payload)

        #case .partial_vector |vector_t|:
            len := val.len()
            var elems Ptr[vector_t.elem] = as (@bitCast(int, val) + 8)
            #if type.managed(vector_t.elem):
                for 0..len |i|:
                    @destruct(vector_t.elem, *elems[i])

        #case .vector |vector_t|:
            elems := as[Ptr[vector_t.elem]] val
            #if type.managed(vector_t.elem):
                for 0..#{vector_t.len} |i|:
                    @destruct(vector_t.elem, *elems[i])

        #case .ref |ref_t|:
            obj := (as[Ptr[Ptr[void]]] val).*
            if @releaseOnly(obj):
                @deinitObject[ref_t.child](obj)

        #case .ref_trait |ref_trait|:
            trait_s := (as[Ptr[TraitStruct]] val).*
            if @releaseOnly(as trait_s.impl):
                @destructBox(as trait_s.impl)

        #case .borrow_trait:
            pass

        -- #case @type:
        --     obj := (val as Ptr[Ptr[void]]).*
        --     if @releaseOnly(obj):
        --         @deinitObject(ref_t.child, obj)

        #case .enum:
            pass

        -- NOTE: Nop cases still need to be handled for inner child destruction.
        #case .ptr:
            pass

        #case .borrow:
            pass

        #case .bool:
            pass

        #case .int:
            pass

        #case .raw:
            pass

        #case .error:
            pass

        #case .float:
            pass

        #case .void:
            pass

        #case .func |func_t|:
            #switch func_t.kind:
                #case .ptr:
                    pass
                #case .union:
                    obj := (as[Ptr[Ptr[void]]] val).*
                    if @releaseOnly(obj):
                        @deinitObject[T](obj)
                #else:
                    panic('Unsupported')
        #else:
            panic('Unsupported destructor for type: ' + type.name(T))

type MapEntry[K Any, V Any]:
    key   K
    value V

fn AutoHash[K Any](key K) -> int:
    #if K == str:
        return Wyhash.hash(0, key.as_ptr_span())
    #else K == int:
        return splitmix64_hash(key)
    #else K == float:
        return @bitCast(int, key)
    #else K == Object:
        return splitmix64_hash(as key)
    #else type.is_instance_of(K, Array):
        meta.error('Hashing an `Array` key is ambiguous. Consider using `ArrayMemMap` or `HashMap`.')
    #else type.is_instance_of(K, Map):
        meta.error('Hashing a `Map` key is ambiguous. Consider using `HashMap`.')
    #else:
        #switch type.info(K):
            #case .enum:
                return splitmix64_hash(key)
            #case .ref:
                return splitmix64_hash(@ref_addr(key))
            #else:
                meta.error('Unsupported: ' + type.name(K))

fn AutoEq[K Any](a K, b K) -> bool:
    #if K == str:
        return a == b
    #else K == int:
        return a == b
    #else K == Object:
        return a == b
    #else:
        return a == b

fn splitmix64_hash(x_ int) -> int:
    x := x_ + 0x9e3779b97f4a7c15
    x ~= x >> 30
    x = x.umul(0xbf58476d1ce4e5b9)
    x ~= x >> 27
    x = x.umul(0x94d049bb133111eb)
    x ~= x >> 31
    return x

--| Wyhash implementation ported from Zig's std.
type Wyhash:
    a       int
    b       int
    state   [3]int
    cur_len int

    buf     [48]byte
    buf_len int

global Wyhash :: secret [4]int = {
    0xa0761d6478bd642f,
    0xe7037ed1a0b428db,
    0x8ebc6af09c88c6e3,
    0x589965cc75374cc3,
}

fn Wyhash :: @init(seed int) -> Wyhash:
    new := Wyhash{
        a       = 0,
        b       = 0,
        state   = {0} ** 3,
        cur_len = 0,
        buf     = {0} ** 48,
        buf_len = 0,
    }
    new.state[0] = seed ~ Wyhash.mix(seed ~ Wyhash.secret[0], Wyhash.secret[1])
    new.state[1] = new.state[0]
    new.state[2] = new.state[0]
    return move new

--@inline
fn Wyhash :: hash(seed int, input AsSpan[byte]) -> int:
    input_span := input.span()
    new := Wyhash(seed)

    if input.len() <= 16:
        new.smallKey(input_span)
    else:
        i := 0
        if input_span.length >= 48:
            while i + 48 < input_span.length:
                new.round(*input_span[i])
                i += 48
            new.final0()
        new.final1(input_span, i)

    new.cur_len = input_span.length
    return new.final2()

--| This is subtly different from other hash function update calls. Wyhash requires the last
--| full 48-byte block to be run through final1 if is exactly aligned to 48-bytes.
fn (&Wyhash) update(input [&]byte):
    $cur_len += input.length

    if input.length <= 48 - $buf_len:
        $buf[$buf_len..][0..input.length].set(input)
        $buf_len += input.length
        return

    i := 0
    if $buf_len > 0:
        i = 48 - $buf_len
        $buf[$buf_len..][0..i].set(input[0..i])
        $round(*$buf[0])
        $buf_len = 0

    while i + 48 < input.length:
        $round(*input[i])
        i += 48

    remaining_bytes := input[i..]
    if remaining_bytes.length < 16 and i >= 48:
        rem := 16 - remaining_bytes.length
        $buf[$buf.len()-rem..].set(input[i-rem..i])
    $buf[0..remaining_bytes.length].set(remaining_bytes)
    $buf_len = remaining_bytes.length

fn (&Wyhash) final() -> int:
    input := $buf[0..$buf_len]
    newSelf := $shallowCopy() -- ensure idempotency

    if $cur_len <= 16:
        newSelf.smallKey(input)
    else:
        offset := 0
        if $buf_len < 16:
            scratch := [16]byte(0)
            rem := 16 - $buf_len
            scratch[0..rem].set($buf[$buf.len()-rem..][0..rem])
            scratch[rem..][0..$buf_len].set($buf[0..$buf_len])

            -- Same as input but with additional bytes preceeding start in case of a short buffer
            input = scratch[0..16]
            offset = rem

        newSelf.final0()
        newSelf.final1(input, offset)

    return newSelf.final2()

--@inline
--| Copies the core wyhash state but not any internal buffers.
fn (&Wyhash) shallowCopy() -> Self:
    return {
        a       = $a,
        b       = $b,
        state   = $state,
        cur_len = $cur_len,
        buf     = {0} ** 48,
        buf_len = 0,
    }

--@inline
fn (&Wyhash) smallKey(input [&]byte):
    assert(input.length <= 16)

    if input.length >= 4:
        end := input.length - 4
        quarter := (input.length >> 3) << 2
        $a = (@zext(int, Wyhash.readI32(*input[0])) << 32) || @zext(int, Wyhash.readI32(*input[quarter]))
        $b = (@zext(int, Wyhash.readI32(*input[end])) << 32) || @zext(int, Wyhash.readI32(*input[end - quarter]))
    else input.length > 0:
        $a = (@zext(int, input[0]) << 16) || (@zext(int, input[input.length >> 1]) << 8) || @zext(int, input[input.length - 1])
        $b = 0
    else:
        $a = 0
        $b = 0

--@inline
fn (&Wyhash) round(input Ptr[byte]):
    a := 0
    b := 0
    #for 0..3 |i|:
        a = Wyhash.readInt(*input[8 * (2 * i)])
        b = Wyhash.readInt(*input[8 * (2 * i + 1)])
        $state[i] = Wyhash.mix(a ~ Wyhash.secret[i + 1], b ~ $state[i])

fn Wyhash :: rot(x int) -> int:
    return (x >> 32) || (x << 32)

--@inline
--| 32-bit mum
fn Wyhash :: mum(a Ptr[int], b Ptr[int]):
    hh := (a.* >> 32).umul(b.* >> 32)
    hl := (a.* >> 32).umul(b.* && 0xffffffff)
    lh := (a.* && 0xffffffff).umul(b.* >> 32)
    ll := (a.* && 0xffffffff).umul(b.* && 0xffffffff)
    a.* = Wyhash.rot(hl) ~ hh
    b.* = Wyhash.rot(lh) ~ ll

--@inline
fn Wyhash :: mix(a int, b int) -> int:
    Wyhash.mum(*a, *b)
    return a ~ b

fn Wyhash :: readInt(ptr Ptr[byte]) -> int:
    return int.decode(ptr, .little)

fn Wyhash :: readI32(ptr Ptr[byte]) -> i32:
    return i32.decode(ptr, .little)

--@inline
fn (&Wyhash) final0():
    $state[0] ~= $state[1] ~ $state[2]

--@inline
--| input_lb must be at least 16-bytes long (in shorter key cases the smallKey function will be
--| used instead). We use an index into a slice to for comptime processing as opposed to if we
--| used pointers.
fn (&Wyhash) final1(input_lb [&]byte, start_pos int):
    assert(input_lb.length >= 16)
    assert(input_lb.length - start_pos <= 48)
    input := input_lb[start_pos..]

    i := 0
    while i + 16 < input.length:
        $state[0] = Wyhash.mix(Wyhash.readInt(*input[i]) ~ Wyhash.secret[1], Wyhash.readInt(*input[i + 8]) ~ $state[0])
        i += 16

    $a = Wyhash.readInt(*input_lb[input_lb.length - 16])
    $b = Wyhash.readInt(*input_lb[input_lb.length - 8])

--@inline
fn (&Wyhash) final2() -> int:
    $a ~= Wyhash.secret[1]
    $b ~= $state[0]
    Wyhash.mum(*$a, *$b)
    return Wyhash.mix($a ~ Wyhash.secret[0] ~ $cur_len, $b ~ Wyhash.secret[1])

#[bind]
type StrBuffer:
    inner RawBuffer[byte]

fn (&StrBuffer) @size() -> int:
    return $inner.@size()

--| TODO: Mutable string type.
#[bind]
type Str:
    inner Array[byte]

--| Immutable string type. Short strings are interned.
#[bind]
type str:
    -- When `buf` is none, the string is a slice of a static string.
    buf ?^StrBuffer
    ptr Ptr[byte]

    --| Contains the byte length and whether the string is ASCII.
    --| The ASCII flag only serves as an optimization hint. When false, the string can still contain just ASCII characters.
    --| `len()` returns the byte length.
    --| `count()` returns the number of runes.
    header int

fn str :: @init(slice []byte) -> str:
    return str(slice.as_ptr_span())

fn str :: @init(span [&]byte) -> str:
    return str(as[PtrSpan[byte]] span)

--| Returns a `str` copied from the byte slice.
#[bind='str.initPtrSpan']
fn str :: @init(span PtrSpan[byte]) -> str

fn str :: @init(x %T) -> str:
    #if T == bool:
        return if (x) 'true' else 'false'
    #else T == byte:
        return x.fmt(.hex)
    #else T == r16:
        return x.fmt(.hex)
    #else T == r32:
        return x.fmt(.hex)
    #else T == r64:
        return x.fmt(.hex)
    #else T == i8:
        return x.fmt()
    #else T == i16:
        return x.fmt()
    #else T == i32:
        return x.fmt()
    #else T == int:
        return x.fmt()
    #else T == float:
        return x.fmt()
    #else T == f32:
        return x.fmt()
    #else T == str:
        return x
    #else T == error:
        return 'error.' + x.sym().name()
    #else T == symbol:
        return '@' + x.name()
    #else type.implements(T, AsSpan[byte]):
        return str(x[..])
    #else:
        meta.error('Unsupported conversion to `str`: ' + type.name(T))

--| Converts a rune to a string.
#[bind]
fn str :: initRune(val int) -> str:
    panic('Unsupported')

--| Reallocates with zero sentinel for C compatibility.
--| The sentinel does not count as a character in the string.
fn str :: initz(src str) -> str:
    len := src.len()
    res := if (src.isAscii()) @init_astr_undef(len + 1)
        else @init_ustr_undef(len + 1)
    @memcpy(res.ptr, src.ptr, len)
    res.ptr[len] = 0
    if src.isAscii():
        res.header = len || (1 << 63)
    else:
        res.header = len
    return move res

fn str :: interpolate(strs [&]str, args [&]str) -> str:
    res := strs[0]
    for args |i, arg|:
        res += arg
        res += strs[i+1]
    return res

--| Returns whether a byte is an ASCII alphabetic letter.
fn str :: is_ascii_alpha(ch byte) -> bool:
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z')

--| Returns whether a byte is an ASCII digit.
fn str :: is_ascii_digit(ch byte) -> bool:
    return ch >= '0' and ch <= '9'

fn str :: is_ascii_lower(ch byte) -> bool:
    return ch >= 'a' and ch <= 'z'

fn str :: is_ascii_upper(ch byte) -> bool:
    return ch >= 'A' and ch <= 'Z'

fn str :: ascii_lower(ch byte) -> byte:
    mask := byte(str.is_ascii_upper(ch)) << 5
    return ch || mask

fn str :: ascii_upper(ch byte) -> byte:
    mask := byte(str.is_ascii_lower(ch)) << 5
    return ch ~ mask

fn (&str) @send(thread Thread) -> str:
    len := $len()
    res := if ($isAscii()) @init_astr_undef(thread, len)
        else @init_ustr_undef(thread, len)
    @memcpy(res.ptr, $ptr, len)
    return move res

--| Returns the byte value (0-255) at the given index `idx`.
#[bind]
fn (&str) @index(idx int) -> byte:
    if idx >= $len():
        panic_oob(idx)
    return $ptr[idx]

--| Returns a new string that concats this string and `s`.
#[bind]
fn (&str) `+`(s str) -> str:
    return $concat(s)

fn (&str) `**`(n int) -> str:
    return $repeat(n)

--| Returns a substring from `start` (inclusive) to `end` (exclusive) byte indexes.
#[bind]
fn (&str) @slice(start int) -> str:
    return $[start..$len()]

--| Returns a substring from `start` (inclusive) to `end` (exclusive) byte indexes.
#[bind='str.@slice2']
fn (&str) @slice(start int, end int) -> str:
    length := $len()
    if start > length:
        panic_oob(start)
    if end > length:
        panic_oob(end)
    if end < start:
        panic('InvalidArgument')

    new_len := end - start
    if $hasAsciiFlag():
        new_len = new_len || (1 << 63)
    return {
        buf    = $buf,
        ptr    = $ptr + start,
        header = new_len,
    }

fn (&str) to_bytes() -> []byte:
    len := $len()
    new := []byte(len, 0)
    @memcpy(new.ptr, $ptr, len)
    return new

#[unsafe]
fn (&str) as_ptr_span() -> PtrSpan[byte]:
    return {
        ptr = $ptr,
        length = $len(),
    }

--| Returns the rune at byte index `idx`. The replacement character (0xFFFD) is returned for an invalid UTF-8 rune.
#[bind]
fn (&str) rune_at(idx int) -> r32:
    if idx.uge($len()):
        panic('OutOfBounds')
    if $hasAsciiFlag():
        return $ptr[idx]
    else:
        if !utf8Check($ptr + idx, $len() - idx):
            return 0xFFFD
        return utf8Decode($ptr + idx)

--| Source: https://github.com/sheredom/utf8.h
fn utf8Check(s Ptr[byte], remaining int) -> bool:
    if 0xf0 == (0xf8 && s[0]):
        -- ensure that there's 4 bytes or more remaining
        if remaining < 4:
            return false

        -- ensure each of the 3 following bytes in this 4-byte
        -- utf8 codepoint began with 0b10xxxxxx
        if (0x80 != (0xc0 && s[1])) or (0x80 != (0xc0 && s[2])) or 
            (0x80 != (0xc0 && s[3])):
            return false

        -- ensure that our utf8 codepoint ended after 4 bytes
        if remaining != 4 and (0x80 == (0xc0 && s[4])):
            return false

        -- ensure that the top 5 bits of this 4-byte utf8
        -- codepoint were not 0, as then we could have used
        -- one of the smaller encodings
        if (0 == (0x07 && s[0])) and (0 == (0x30 && s[1])):
            return false
  
        -- 4-byte utf8 code point (began with 0b11110xxx)
        return true

    else 0xe0 == (0xf0 && s[0]):
        -- ensure that there's 3 bytes or more remaining
        if remaining < 3:
            return false
  
        -- ensure each of the 2 following bytes in this 3-byte
        -- utf8 codepoint began with 0b10xxxxxx
        if (0x80 != (0xc0 && s[1])) or (0x80 != (0xc0 && s[2])):
            return false
  
        -- ensure that our utf8 codepoint ended after 3 bytes
        if remaining != 3 and (0x80 == (0xc0 && s[3])):
            return false
  
        -- ensure that the top 5 bits of this 3-byte utf8
        -- codepoint were not 0, as then we could have used
        -- one of the smaller encodings
        if (0 == (0x0f && s[0])) and (0 == (0x20 && s[1])):
            return false
  
        -- 3-byte utf8 code point (began with 0b1110xxxx)
        return true

    else 0xc0 == (0xe0 && s[0]):
        -- ensure that there's 2 bytes or more remaining
        if remaining < 2:
            return false
  
        -- ensure the 1 following byte in this 2-byte
        -- utf8 codepoint began with 0b10xxxxxx
        if 0x80 != (0xc0 && s[1]):
            return false
  
        -- ensure that our utf8 codepoint ended after 2 bytes
        if remaining != 2 and (0x80 == (0xc0 && s[2])):
            return false
  
        -- ensure that the top 4 bits of this 2-byte utf8
        -- codepoint were not 0, as then we could have used
        -- one of the smaller encodings
        if 0 == (0x1e && s[0]):
            return false
  
        -- 2-byte utf8 code point (began with 0b110xxxxx)
        return true

    else 0x00 == (0x80 && s[0]):
        -- 1-byte ascii (began with 0b0xxxxxxx)
        return true

    else:
        -- we have an invalid 0b1xxxxxxx utf8 code point entry
        return false

fn utf8Decode(s Ptr[byte]) -> r32:
    if 0xf0 == (0xf8 && s[0]):
        -- 4 byte utf8 codepoint
        return ((0x07 && s[0]) << 18) || ((0x3f && s[1]) << 12) ||
                    ((0x3f && s[2]) << 6) || (0x3f && s[3])
        -- str += 4
    else 0xe0 == (0xf0 && s[0]):
        -- 3 byte utf8 codepoint
        return ((0x0f && s[0]) << 12) || ((0x3f && s[1]) << 6) || (0x3f && s[2])
        -- str += 3
    else 0xc0 == (0xe0 && s[0]):
        -- 2 byte utf8 codepoint
        return ((0x1f && s[0]) << 6) || (0x3f && s[1])
        -- str += 2
    else:
        -- 1 byte utf8 codepoint otherwise
        return s[0]
        -- str += 1

fn utf8SeqLen(first byte) -> int:
    if 0xf0 == (0xf8 && first):
        return 4
    else 0xe0 == (0xf0 && first):
        return 3
    else 0xc0 == (0xe0 && first):
        return 2
    else:
        return 1

fn (&str) beforeLast() -> str:
    return $[0..$len()-1]

--| Returns a new string that concats this string and `s`.
#[bind]
fn (&str) concat(s str) -> str:
    len := $len()
    s_len := s.len()
    res := if ($isAscii() and s.isAscii()) @newAstrUndef(len + s_len)
        else @newUstrUndef(len + s_len)
    @memcpy(res.ptr, $ptr, len)
    @memcpy(res.ptr + len, s.ptr, s_len)
    return move res

fn (&str) contains(needle str) -> bool:
    return $index(needle) != none

-- Compare lexicographically.
fn (str) compare(right str) -> CompareOrder:
    n := if ($len() < right.len()) $len() else right.len()
    for 0..n |i|:
        left_byte := $ptr[i]
        right_byte := right.ptr[i]
        if left_byte < right_byte:
            return .lt
        else left_byte > right_byte:
            return .gt
    if $len() < right.len():
        return .lt
    else $len() > right.len():
        return .gt
    else:
        return .eq

--| Returns the number of runes in the string.
#[bind]
fn (&str) count() -> int:
    len := $len()
    idx := 0
    res := 0
    while idx < len:
        cp_len := utf8SeqLen($ptr[idx])
        res += 1
        idx += cp_len
    return res

--| Calls decode(@utf8)
-- #[bind] fn str.decode(self str) -> str

--| Decodes the array based on an `encoding`. Supported encodings: `.utf8`.
--| Returns the decoded string or throws `error.Decode`.
-- #[bind='str.decode2']
-- fn str.decode(self str, encoding symbol) -> str

--| Returns whether the string ends with `suffix`.
#[bind]
fn (&str) ends_with(suffix str) -> bool:
    return $ptr[0..$len()].ends_with(suffix.ptr[0..suffix.len()])

--| Returns the first byte index of substring `needle` in the string or `none` if not found. SIMD accelerated.
#[bind]
fn (&str) index(needle str) -> ?int:
    end := $len() - needle.len() + 1
    first := needle[0]
    for 0..end |i|:
        if $ptr[i] == first:
            if $[i..i+needle.len()] == needle:
                return i
    return none

--| Returns the first byte index of a rune `needle` in the string or `none` if not found. SIMD accelerated.
#[bind]
fn (&str) index_rune(needle int) -> ?int:
    len := $len()
    i := 0
    while i < len:
        rune_len := utf8SeqLen($ptr[i])
        rune := utf8Decode($ptr + i)
        if rune == needle:
            return i
        i += rune_len
    return none

--| Returns the first index of `byte` in the array or `none` if not found.
#[bind='str.index_byte']
fn (&str) index(b byte) -> ?int:
    for 0..$len() |i|:
        if $ptr[i] == b:
            return i
    return none

--| Returns the first byte index of `\n` or `\r` or `none`. SIMD accelerated.
#[bind]
fn (&str) index_newline() -> ?int

--| Returns the first index of any byte in `set` or `none` if not found.
#[bind='str.index_any_byte']
fn (&str) index_any(set [&]byte) -> ?int:
    for 0..$len() |i|:
        if set.index($ptr[i]) != none:
            return i
    return none

fn (&str) index_any(set AsSpan[byte]) -> ?int:
    return $index_any(set.span())

--| Returns the first byte index of any rune in `runes` or `none` if not found. SIMD accelerated.
#[bind='str.index_any_rune']
fn (&str) index_any_rune(runes [&]int) -> ?int:
    len := $len()
    i := 0
    while i < len:
        rune_len := utf8SeqLen($ptr[i])
        rune := utf8Decode($ptr + i)
        if runes.index(rune) != none:
            return i
        i += rune_len
    return none

fn (&str) index_any_rune(runes AsSpan[int]) -> ?int:
    return $index_any(runes.span())

fn (&str) iterator() -> int:
    return 0

--| Replaces each placeholder `{}` from the receiver string with the corresponding value in `args` converted to a string.
--| TODO: This should accept tuple instead of []Object.
fn (&str) fmt(args %T) -> str:
    return $fmt('{}', move args)

fn (&str) fmt(placeholder str, args %T) -> str:
    res := ''
    arg_idx := 0
    cur := 0

    #struct_t := type.info(T).!struct
    #for 0..struct_t.fields.len() |i|:
        begin:
            #field := struct_t.fields[i]

            found_idx := $[cur..].index(placeholder) ?else:
                panic('Not enough placeholders. %{i}')

            res += $[cur..cur+found_idx] + to_print_string(meta.access(args, #{field.name}))
            cur += found_idx + placeholder.len()

    if cur < $len():
        if $[cur..].index(placeholder) != none:
            panic('Not enough arguments.')
        res += $[cur..]

    return res

--| Formats each byte in the string using a NumberFormat.
--| Each byte is zero padded.
#[bind]
fn (&str) fmtBytes(format NumberFormat) -> str:
    res := ''
    for 0..$len() |i|:
        res += $[i].fmt(format)
    return res

fn (&str) hasAsciiFlag() -> bool:
    return ($header && (1 << 63)) != 0

fn (&str) intAt(idx int) -> int:
    return int.decode($ptr + idx, .little)

fn (&str) intAt(idx int, endian Endian) -> int:
    return int.decode($ptr + idx, endian)

fn (&str) i32At(idx int) -> i32:
    return i32.decode($ptr + idx, .little)

fn (&str) i32At(idx int, endian Endian) -> i32:
    return i32.decode($ptr + idx, endian)

--| Returns a new string with `str` inserted at byte index `idx`.
#[bind]
fn (&str) insert(idx int, s str) -> str:
    return $[0..idx] + s + $[idx..]

--| Returns a new array with `byte` inserted at index `idx`.
#[bind='str.insert_byte']
fn (&str) insert(idx int, b byte) -> str:
    len := $len()
    is_ascii := b < 0x80
    res := if (@isAscii() and is_ascii) @newAstrUndef(len + 1)
        else @newUstrUndef(len + 1)
    @memcpy(res.ptr, $ptr, idx)
    res.ptr[idx] = b
    if idx < len:
        @memcpy(res.ptr + idx + 1, $ptr + idx, len - idx)
    return move res

--| Returns whether the string contains all ASCII runes.
--| Every time this function is called a full scan is performed.
#[bind]
fn (&str) isAscii() -> bool:
    if @hasAsciiFlag():
        return true
    -- TODO: Port SIMD case.
    for 0..$len() |i|:
        if $[i] && 0x80 > 0:
            return false
    return true

fn (&str) last() -> byte:
    return $[$len()-1]

fn (&str) lastIndexOf(needle str) -> ?int:
    start := $len() - needle.len()
    first := needle[0]
    for start..>=0 |i|:
        if $ptr[i] == first:
            if $[i..i+needle.len()] == needle:
                return i
    return none

--| Returns the byte length of the string. See `count()` to obtain the number of runes.
#[bind]
fn (&str) len() -> int:
    return $header && ~(1 << 63)

--| Returns whether this string is lexicographically before `other`.
#[bind]
fn (str) less(other str) -> bool:
    return @compare(other) == .lt

--| Returns this string in lowercase.
#[bind]
fn (&str) lower() -> str:
    len := $len()
    res := if (@isAscii()) @newAstrUndef(len) else @newUstrUndef(len)
    for 0..len |i|:
        res.ptr[i] = $ptr[i].lower()
    return res

fn (&str) next(idx &int) -> ?byte:
    if idx.*.uge($len()):
        return none
    res := $[idx.*]
    idx.* += 1
    return res

--| Returns a new string with all occurrences of `needle` replaced with `replacement`.
#[bind]
fn (&str) replace(needle str, replacement str) -> str:
    res := ''
    i := 0
    while $[i..].index(needle) |found_idx|:
        res += $[i..i+found_idx] + replacement
        i += found_idx + needle.len()

    if i < $len():
        res += $[i..]

    return res

--| Returns a new string with this string repeated `n` times.
#[bind]
fn (&str) repeat(n int) -> str:
    res := ''
    for 0..n:
        res += $.*
    return res

--| Returns the n'th rune.
fn (&str) seek(n int) -> r32:
    return $rune_at($seek_pos(n))

--| Returns the starting byte index for the n'th rune.
#[bind]
fn (&str) seek_pos(n int) -> int:
    len := $len()
    i := 0
    rune_i := 0
    while i < len:
        if rune_i == n:
            return i
        rune_len := utf8SeqLen($ptr[i])
        rune := utf8Decode($ptr + i)
        i += rune_len
        rune_i += 1

    if rune_i == n:
        return i
    panic('OutOfBounds')

--| Returns the UTF-8 rune starting at byte index `idx` as a string.
#[bind]
fn (&str) runeStrAt(idx int) -> str:
    rune_len := utf8SeqLen($ptr[idx])
    return $[idx..idx+rune_len]

fn (&str) set(idx int, x byte) -> str:
    return $[0..idx] + str.initRune(x) + $[idx+1..]

--| Returns a list of UTF-8 strings split at occurrences of `sep`.
fn (&str) split(sep str) -> []str:
    res := []str{}
    if $len() == 0:
        res += ''
        return res

    i := 0
    while $[i..].index(sep) |found_idx|:
        res += $[i..i+found_idx]
        i += found_idx + sep.len()

    if i < $len():
        res += $[i..]

    return res

--| Returns whether the `str` starts with `prefix`.
#[bind]
fn (&str) starts_with(prefix str) -> bool:
    return $ptr[0..$len()].starts_with(prefix.ptr[0..prefix.len()])

fn (&str) trimLeft(delim str) -> str:
    res := $.*
    while res.len() >= delim.len():
        i := delim.len()
        if res[0..i] != delim:
            break
        res = res[i..]
    return res

fn (&str) trimLeft(delims AsSpan[str]) -> str:
    res := $.*
    for delims |delim|:
        res = res.trimLeft(delim)
    return res

fn (&str) trimRight(delim str) -> str:
    res := $.*
    while res.len() >= delim.len():
        i := res.len() - delim.len()
        if res[i..] != delim:
            break
        res = res[0..i]
    return res

fn (&str) trimRight(delims AsSpan[str]) -> str:
    res := $.*
    for delims |delim|:
        res = res.trimRight(delim)
    return res

--| Returns the string with both ends trimmed.
fn (&str) trim(delim str) -> str:
    return $trimLeft(delim).trimRight(delim)

fn (&str) trim(delims AsSpan[str]) -> str:
    return $trimLeft(delims).trimRight(delims)

--| Returns this string in uppercase.
#[bind]
fn (&str) upper() -> str:
    len := $len()
    res := if ($isAscii()) @newAstrUndef(len) else @newUstrUndef(len)
    for 0..len |i|:
        res.ptr[i] = $ptr[i].upper()
    return res

#[bind] 
type GenericVector[T Any] _

#[bind]
type PartialVector[T Any, const N int] _

fn (scope &PartialVector[]) @index_addr(idx int) -> scope &T:
    if idx.uge($len()):
        panic_oob(idx)
    elem_addr := @unsafeCast(int, $) + 8
    return @scope_ptr_to_borrow('self', as[Ptr[T]] elem_addr + idx)

--| Alias for `append`.
fn (&PartialVector[]) `<<`(x T):
    Self.append($, x)

fn (&PartialVector[]) append(x T):
    _len := $len()
    if _len == N:
        panic('`%{type.name(Self)}` already has the max number of elements `%{$len()}`.')
    addr := @unsafeCast(int, $)
    @ptr_init((as[Ptr[T]] (addr + 8)) + _len, x)
    (as[Ptr[int]] addr).* += 1

--| Returns the number of elements in the partial array.
fn (&PartialVector[]) len() -> int:
    -- Get addr first to avoid implicit elem[0] cast for `Ptr[int]`.
    addr := @unsafeCast(int, $)
    return (as[Ptr[int]] addr).*

#[unsafe]
fn (&PartialVector[]) setLength(len int):
    addr := @unsafeCast(int, $)
    (as[Ptr[int]] addr).* = len

#[bind]
type Vector[T Any, const N int] _

fn Vector[] :: @init(elem T) -> Self:
    return [1]T{elem} ** N

fn (&Vector[]) @copy() -> Self:
    new := as[Self] undef
    for 0..N |i|:
        @ptr_init(*new[0] + i, $[i])
    return move new

fn (scope &Vector[]) @index_addr(idx int) -> scope &T:
    if idx.uge(N):
        panic_oob(idx)
    return @scope_ptr_to_borrow('self', as[Ptr[T]] $ + idx)

fn (scope &Vector[]) @slice() -> scope [&]T:
    return $[0..N]

fn (scope &Vector[]) @slice(start int) -> scope [&]T:
    return $[start..N]

--| Returns a slice of this vector from `start` (inclusive) to `end` (exclusive) indexes.
fn (scope &Vector[]) @slice(start int, end int) -> scope [&]T:
    if start.ugt(N):
        panic_oob(start)
    if end.ugt(N):
        panic_oob(end)
    if end.ult(start):
        panic('InvalidArgument')
    return {
        base   = as *$[start],
        length = end - start,
    }

fn (&Vector[]) `+`(o Vector[T, %M]) -> Vector[T, N + M]:
    new := as[Vector[T, N + M]] undef
    new[0..N].init($[0..])
    new[N..].init(o)
    return move new

#[bind]
fn (&Vector[]) ct_repeat(%M int) -> Vector[T, N * M]

fn (&Vector[]) `**`(%M int) -> Vector[T, N * M]:
    #if meta.is_inline_eval():
        return $ct_repeat(M)
    #else:
        var new Vector[T, N*M] = undef
        #if !type.managed(T):
            @memset(as *new[0], as *$[0], type.size(T) * N, M)
        #else:
            for 0..M |i|:
                new[i*N..i*N+N].init($[0..])
        return move new

-- --| Returns a new array that concats this array and `other`.
-- #[bind] fn Vector[].concat(self, other Vector[#M, T]) Vector[N + M, T]

-- --| Returns a new array with `arr` inserted at index `idx`.
-- #[bind] fn Vector[].insert(self, idx int, arr Vector) Vector

--| Returns a new iterator over the array.
fn (&Vector[]) iterator() -> int:
    return 0

--| Returns the number of elements in the array.
fn (&Vector[]) len() -> int:
    return N

fn (&Vector[]) next(idx &int) -> ?T:
    if idx.* >= N:
        return none
    res := $[idx.*]
    idx.* += 1
    return res

#[unsafe]
fn (&Vector[]) as_ptr_span() -> PtrSpan[T]:
    return {
        ptr = *$[0],
        length = N,
    }

--| Unsafe pointer type.
--| NOTE: Should not have any regular methods because they could shadow child methods.
#[bind]
type Ptr[T Any] _

fn Ptr[] :: @init(x %T) -> Self:
    #if false:
        -- Placeholder for specific types.
        pass
    #else:
        #switch type.info(T):
            #case .raw |info|:
                #if type.size(T) < type.size(Self):
                    return @bitCast(Self, @zext(Raw[type.size(Self) * 8], x))
                #else type.size(T) > type.size(Self):
                    if x > Raw[type.size(Self)*8].ones:
                        panic('Lossy conversion from `%{x}` to `%{type.name(Self)}`')
                    return @bitCast(Self, @bitTrunc(Raw[type.size(Self)*8], x))
                #else:
                    return @bitCast(Self, x)
            #case .int |info|:
                #if type.size(T) < type.size(Self):
                    return @bitCast(Self, @zext(Raw[type.size(Self) * 8], x))
                #else type.size(T) > type.size(Self):
                    if Raw[info.bits](x) > Raw[type.size(Self)*8].ones:
                        panic('Lossy conversion from `%{x}` to `%{type.name(Self)}`')
                    return @bitCast(Self, @bitTrunc(Raw[type.size(Self)*8], x))
                #else:
                    return @bitCast(Self, x)
            #else:
                meta.error('Unsupported: ' + type.name(T))

#[reserve]
fn (Ptr[]) @index_addr(idx int) -> Ptr[T]

#[bind]
fn @ptr_init(ptr Ptr[%T], value T)

#[bind]
fn @ref_addr(ref ^%T) -> int

fn (Ptr[]) @slice(start int, end int) -> PtrSpan[T]:
    return {
        ptr    = $ + start,
        length = end - start,
    }

#[bind]
fn (Ptr[]) `+`(offset int) -> Ptr[T]:
    return *$[offset]

fn (Ptr[]) `-`(right Self) -> int:
    return ((as[int] $) - (as[int] right)) / type.size(T)

fn (Ptr[]) `<`(right Self) -> bool:
    return ((as[int] $) < (as[int] right))

--| Sets the value assuming the underlying memory is uninitialized.
-- fn Ptr[].init(self Self, val T):
--     $init_(type.id(T), val)

-- #[bind] fn Ptr[].init_(self Self, type_id int, val T)

#[bind]
type Range:
    start int
    end   int

#[bind]
type TccState:
    inner Ptr[void]

#[bind] type Result[T Any] _

fn Result[] :: @init(x Self) -> Self:
    return x

--| Returns the Result's successful value case or panics.
fn (&Result[]) unwrap() -> T:
    return @result_unwrap(T, self)

#[bind]
fn @result_unwrap(%T type, res Code) -> T

--| Returns the Result's error case or panics.
fn (&Result[]) unwrap_error() -> error:
    return @result_unwrap_error(self)

#[bind]
fn @result_unwrap_error(res Code) -> error

#[bind]
type Option[T Any] _

fn Option[] :: @init(x Self) -> Self:
    return x

#[bind]
fn @await_(fut &FutureValue[%T])

#[bind]
fn @notifyFutureComplete(f Future[%T])

type Future[T Any]:
    inner ^FutureValue[T]

fn (&Future[]) await() -> T:
    @await_(as[&FutureValue[T]] $.inner)
    return meta.access_option_payload($inner.data)

type FutureValue[T Any]:
    cont_head int -- index to `vm.task_nodes` or NullId
    cont_tail int

    -- `none` if not shared.
    shared_state Ptr[byte]
    completed bool

    data      ?T

-- type AsyncTask[T Any]:
--     cb   FutureOnResultFn[T]
--     next ?^FutureTask

-- type FutureOnResultFn[T Any] = Fn(res T)

-- fn FutureValue.onResult(cb FutureOnResultFn[T]):
--     task := ^FutureTask[T]{
--         cb = cb,
--         next = none,
--     }
--     if $cont_head == none:
--         $cont_head = task
--         $cont_tail = task
--         return

--     $cont_tail.?.next = task
--     $cont_tail = task

const NullId = (1 << 32) - 1

--| Returns a `Future[T]` that has a completed value.
fn Future :: complete(val %T) -> Future[T]:
    return {
        inner = ^FutureValue[T]{
            cont_head = NullId,
            cont_tail = NullId,
            completed = true,
            shared_state = none,
            data = val,
        }
    }
    
fn Future :: @init(%T type) -> Future[T]:
    return {
        inner = ^FutureValue[T]{
            cont_head = NullId,
            cont_tail = NullId,
            shared_state = none,
            completed = false,
            data = none,
        }
    }

type FutureResolver[T Any]:
    future_ Future[T]

fn FutureResolver :: new(%T type) -> FutureResolver[T]:
    return {future_=Future(T)}

fn (&FutureResolver[]) complete(val T):
    if $future_.inner.completed:
        panic('Future is already completed.')

    $future_.inner.completed = true
    $future_.inner.data = val
    @notifyFutureComplete($future_)

fn (&FutureResolver[]) future() -> Future[T]:
    return $future_

#[bind]
type Ref[T Any] _

#[bind]
type Borrow[T Any] _

#[bind]
type ExBorrow[T Any] _

type PtrSpan[T Any]:
    with AsSpan[T]

    ptr    Ptr[T] = none
    length int    = 0

fn (&PtrSpan[]) @index_addr(idx int) -> Ptr[T]:
    if idx.uge($length):
        panic_oob(idx)
    return *$ptr[idx]

fn (&PtrSpan[]) @slice(start int) -> PtrSpan[T]:
    return $ptr[start..$length]

fn (&PtrSpan[]) @slice(start int, end int) -> PtrSpan[T]:
    if start.ugt($length):
        panic_oob(start)
    if end.ugt($length):
        panic_oob(end)
    if start.ugt(end):
        panic('InvalidArgument')

    return $ptr[start..end]

#[unsafe]
fn (&PtrSpan[]) span() -> [&]T:
    return as $.*

fn (&PtrSpan[]) @set_index(idx int, val T):
    $ptr[idx] = val

fn (&PtrSpan[]) toBytes() -> PtrSpan[byte]:
    return (as[Ptr[byte]] $ptr)[0..$length * type.size(T)]

--| Returns whether the array ends with `suffix`.
fn (&PtrSpan[]) ends_with(suffix PtrSpan[T]) -> bool:
    if suffix.length > $length:
        return false
    for $[$length-suffix.length..] |i, elem|:
        if elem != suffix[i]:
            return false
    return true

-- --| Returns the first index of `needle` in the slice or `none` if not found.
-- #[bind] fn PtrSpan[].find(self, needle [*]T) -> ?int

-- --| Returns the first index of any `bytes` in `set` or `none` if not found.
-- #[bind] fn PtrSpan[].findAny(self, set [*]T) -> ?int

--| Returns the first index of `needle` in the slice or `none` if not found.
fn (&PtrSpan[]) index(needle T) -> ?int:
    for 0..$length |i|:
        if $[i] == needle:
            return i
    return none

fn (&PtrSpan[]) iterator() -> int:
    return 0

fn (&PtrSpan[]) len() -> int:
    return $length

fn (&PtrSpan[]) next(idx &int) -> ?T:
    if idx.* >= $length:
        return none
    res := $[idx.*]
    idx.* += 1
    return res

-- TODO: Consider returning iterator instead.
-- --| Returns a list of arrays split at occurrences of `sep`.
-- #[bind] fn PtrSpan[].split(self, sep [*]T) -> List[Vector]

--| Returns whether the `PtrSpan` starts with `target`.
fn (&PtrSpan[]) starts_with(target PtrSpan[T]) -> bool:
    return $span().starts_with(as target)

fn (&PtrSpan[]) init(o Self):
    if o.length != $length:
        panic('InvalidLen')

    #if type.managed(T):
        for 0..$length |i|:
            @ptr_init($ptr + i, o[i])
    #else:
        @memcpy(as $ptr, as o.ptr, $length * type.size(T))

fn (&PtrSpan[]) init(val T):
    for 0..$length |i|:
        @ptr_init($ptr + i, val)

fn (&PtrSpan[]) set(o Self):
    if o.length != $length:
        panic('InvalidLen')
    #if type.managed(T):
        for 0..$length |i|:
            $[i] = o[i]
    #else:
        @memmove(as $ptr, as o.ptr, type.size(T) * $length)

fn (&PtrSpan[]) set(val T):
    #if type.managed(T):
        for 0..$length |i|:
            $ptr[i] = val
    #else:
        @memset(as $ptr, as *val, type.size(T), $length)

fn (&PtrSpan[]) set_backwards(o Self):
    if o.length != $length:
        panic('InvalidLen')
    for $length-1..>=0 |i|:
        $[i] = o[i]

-- --| Returns a slice with ends trimmed from `delims`. `mode` can be @left, @right, or @ends.
-- #[bind] fn PtrSpan[].trim(self, mode symbol, delims PtrSpan[T]) PtrSpan[T]

-- For REPL.
#[bind] fn @trackMainLocal(name str, type_id int, addr Ptr[void])

type SystemKind enum:
    case linux
    case macos
    case windows
    case freestanding
    case wasi

#[bind] type VaList[T Any] _

--| A generator is a stackless coroutine that can `yield` values back to the dispatcher.
--| For an isolated execution context, consider using threads.
type Generator[FnPtr Any]:
    frame_ptr Ptr[int]
    frame_len i32
    resume_pc Ptr[byte]
    deinit_pc Ptr[byte]

    -- Since generators can not be resumed while active,
    -- save the prev_fp here and store the generator handle in frame[3]
    prev_fp Ptr[int]
    running bool
    done    bool

fn (&Generator[]) @deinit():
    $end()
    $deinit()

#[bind] fn (&Generator[]) deinit()

#[bind] fn (&Generator[]) @size() -> int

fn (&Generator[]) end():
    @gen_end(self.*)

#[bind]
fn @gen_end(gen Generator[%FnPtr])

fn (&Generator[]) next() -> ?(type.fn_ret(FnPtr)):
    return @gen_next(self.*)

#[bind]
fn @gen_next(gen Generator[%FnPtr]) -> ?(type.fn_ret(FnPtr))

#[bind] fn (&Generator[]) status() -> GeneratorStatus

type GeneratorStatus enum:
    case paused
    case running
    case done

--| Returns the smallest of two values.
fn min(a %T, b T) -> T:
    if a > b:
        return b
    else:
        return a

--| Returns the largest of two values.
fn max(a %T, b T) -> T:
    if a < b:
        return b
    else:
        return a

--| Wraps generic traits for dynamic dispatch.
--| `Dyn` is an opaque type.
#[bind]
type Dyn[T Any] _

--| Handle to a thread. Should only be used internally and in `@send` implementations.
#[bind]
type Thread _

fn spawn(func funcptr_t[%Sig], args FnTuple[Sig]) -> Future[Sig.ret()]:
    thread := @new_thread()

    -- TODO: Keep `thread_args` on the stack.
    var thread_args FnTuple[Sig] = undef
    #for 0..Sig.num_params() |i|:
        #param := Sig.param_at(i)
        #param_name := 'p' + EvalStr(i)
        @ptr_init(*meta.access(thread_args, param_name), @send(thread, *meta.access(args, param_name)))

    future := Future(Sig.ret())
    future.inner.shared_state = @new_shared_state()
    future.inner.shared_state.* = 0

    args_size := type.size(FnTuple[Sig])
    ret_size := type.size(Sig.ret())
    @start_thread(thread, @thread_entry[Sig], func, args_size, move thread_args, future.inner, ret_size)
    return future

#[bind]
fn @new_shared_state() -> Ptr[byte]

#[bind]
fn @new_thread() -> Thread

#[bind]
fn @start_thread(thread Thread, entry funcptr_t[%EntrySig], func funcptr_t[%Sig], args_size int, args FnTuple[Sig], future ^FutureValue[Sig.ret()], ret_size int)

fn @thread_entry[const Sig FuncSig](result_thread Thread, func funcptr_t[Sig], args FnTuple[Sig]) -> Sig.ret():
    res := @call(func, &args)
    -- TODO: Check that `result_thread` is still alive.
    return @send(result_thread, *res)

-- TODO: &FnTuple[Sig] is a temporary fix for inlined builtins. Won't be necessary after struct inlining work.
#[bind]
fn @call(func funcptr_t[%Sig], args &FnTuple[Sig]) -> Sig.ret()