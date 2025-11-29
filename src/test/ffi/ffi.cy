use t 'test'
use meta
use os
use c

const lib_path str = switch meta.system():
    case .linux => 'src/test/ffi/test_lib.so'
    case .windows => 'src/test/ffi/test_lib.dll'
    case .macos => 'src/test/ffi/test_lib.dylib'
    else => meta.unsupported()

#c.bind_lib(lib_path)

-- -- Not found.
-- var ffi = os.newFFI()
-- var lib2 = try ffi.bindLib('xyz123.so')
-- t.eq(lib2, error.FileNotFound)

#[extern]
fn testAdd(a, b int) -> int

#[extern]
fn testI8(a i8) -> i8

#[extern]
fn testU8(a r8) -> r8

#[extern]
fn testI16(a i16) -> i16

#[extern]
fn testU16(a r16) -> r16

#[extern]
fn testI32(a i32) -> i32

#[extern]
fn testU32(a r32) -> r32

#[extern]
fn testI64(a i64) -> i64

#[extern]
fn testU64(a r64) -> r64

#[extern]
fn testUSize(a r64) -> r64

#[extern]
fn testF32(a f32) -> f32

#[extern]
fn testF64(a f64) -> f64

#[extern]
fn testCharPtr(a Ptr[byte]) -> Ptr[byte]

#[extern]
fn testVoidPtr(a Ptr[void]) -> Ptr[void]

#[extern]
fn testVoid() -> void

#[extern]
fn testBool(a bool) -> bool

#[extern]
fn test_struct(a Foo) -> Foo

-- C Arrays are passed as reference.
#[extern]
fn test_array(a Ptr[float]) -> float

type Foo cstruct:
    a float
    b int
    c Ptr[void]
    d bool

t.eq(testAdd(123, 321), 444)
t.eq(testI8(-128), -128)
t.eq(testU8(255), 255)
t.eq(testI16(-32768), -32768)
t.eq(testU16(65535), 65535)
t.eq(testI32(-2147483648), -2147483648)
t.eq(testU32(4294967295), 4294967295)
t.eq(testI64(-123456789000), -123456789000)
t.eq(testU64(123456789000), 123456789000)
t.eq(testUSize(123456789000), 123456789000)
t.eqNear(testF32(1.2345), 1.2345)
t.eq(testF64(1.2345), 1.2345)

arr := [_]float{123, 321}
t.eq(test_array(*arr[0]), 444.0)

-- object arg and return type.
cstr := c.to_strz('foo')
res := test_struct({a=123.0, b=10, c=cstr, d=true})
t.eq(res.a, 123.0)
t.eq(res.b, 10)
t.eq(c.from_strz(res.c), 'foo')
t.eq(res.d, true)
os.free(cstr)

-- testCharPtr
cstr = c.to_strz('foo')
t.eq(c.from_strz(testCharPtr(cstr)), 'foo')
os.free(cstr)

-- testVoidPtr
t.eq(testVoidPtr(as[Ptr[void]] 123), as[Ptr[void]] 123)
t.eq(testVoidPtr(as[Ptr[void]] 0), as[Ptr[void]] 0)

-- void return and no args.
testVoid()

-- bool arg and bool return.
t.eq(testBool(true), true)
t.eq(testBool(false), false)

-- Callback.
type Callback = #[extern] fn(a int, b int) -> int

#[extern]
fn test_callback(a int, b int, cb Callback) -> int

#[extern]
fn add_callback(a int, b int) -> int:
    return a + b

t.eq(test_callback(10, 20, add_callback), 30)

--cytest: pass
