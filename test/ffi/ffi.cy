use t 'test'
use os

-- Not found.
var ffi = os.newFFI()
dyn lib = try ffi.bindLib('xyz123.so')
t.eq(lib, error.FileNotFound)

var libPath ?String = none
if os.system == 'macos':
    -- rdynamic doesn't work atm for MacOS.
    libPath = 'test/ffi/macos_lib.dylib'
else os.system == 'windows':
    libPath = 'test/ffi/win_lib.dll'

type MyObject:
    a float
    b int
    c *void
    d bool

ffi = os.newFFI()
ffi.cbind(MyObject, .{symbol.double, symbol.int, symbol.charPtr, symbol.bool})
ffi.cfunc('testAdd', .{symbol.int, symbol.int}, symbol.int)
ffi.cfunc('testI8', .{symbol.char}, symbol.char)
ffi.cfunc('testU8', .{symbol.uchar}, symbol.uchar)
ffi.cfunc('testI16', .{symbol.short}, symbol.short)
ffi.cfunc('testU16', .{symbol.ushort}, symbol.ushort)
ffi.cfunc('testI32', .{symbol.int}, symbol.int)
ffi.cfunc('testU32', .{symbol.uint}, symbol.uint)
ffi.cfunc('testI64', .{symbol.long}, symbol.long)
ffi.cfunc('testU64', .{symbol.ulong}, symbol.ulong)
ffi.cfunc('testUSize', .{symbol.usize}, symbol.usize)
ffi.cfunc('testF32', .{symbol.float}, symbol.float)
ffi.cfunc('testF64', .{symbol.double}, symbol.double)
ffi.cfunc('testCharPtr', .{symbol.charPtr}, symbol.charPtr)
ffi.cfunc('testVoidPtr', .{symbol.voidPtr}, symbol.voidPtr)
ffi.cfunc('testVoid', .{}, symbol.void)
ffi.cfunc('testBool', .{symbol.bool}, symbol.bool)
ffi.cfunc('testObject', .{MyObject}, MyObject)
ffi.cfunc('testRetObjectPtr', .{MyObject}, symbol.voidPtr)
ffi.cfunc('testArray', .{os.CArray{n=2, elem=symbol.double}}, symbol.double)
lib = ffi.bindLib(libPath)

t.eq(lib['testAdd'](123, 321), 444)
t.eq(lib['testI8'](-128), -128)
t.eq(lib['testU8'](255), 255)
t.eq(lib['testI16'](-32768), -32768)
t.eq(lib['testU16'](65535), 65535)
t.eq(lib['testI32'](-2147483648), -2147483648)
t.eq(lib['testU32'](4294967295), 4294967295)
t.eq(lib['testI64'](-123456789000), -123456789000)
t.eq(lib['testU64'](123456789000), 123456789000)
t.eq(lib['testUSize'](123456789000), 123456789000)
t.eqNear(lib['testF32'](1.2345) as float, 1.2345)
t.eq(lib['testF64'](1.2345), 1.2345)
t.eq(lib['testArray']([2]float{123.0, 321.0}), 444.0)

-- object arg and return type.
var cstr = os.cstr('foo')
dyn res = lib['testObject'](MyObject{a=123.0, b=10, c=cstr, d=true})
t.eq(res.a, 123.0)
t.eq(res.b, 10)
t.eq(res.c.fromCstr(0), 'foo')
t.eq(res.d, true)
os.free(cstr)

-- Return struct ptr and convert to Cyber object.
cstr = os.cstr('foo')
var ptr = lib['testRetObjectPtr'](MyObject{a=123.0, b=10, c=cstr, d=true})
t.eq(type(ptr), *void)
res = lib['ptrToMyObject'](ptr)
t.eq(res.a, 123.0)
t.eq(res.b, 10)
t.eq(res.c.fromCstr(0), 'foo')
t.eq(res.d, true)
os.free(cstr)

-- testCharPtr
cstr = os.cstr('foo')
t.eq(lib['testCharPtr'](cstr).fromCstr(0), 'foo')
os.free(cstr)

-- testVoidPtr
t.eq(lib['testVoidPtr'](pointer.fromAddr(void, 123)), pointer.fromAddr(void, 123))
t.eq(lib['testVoidPtr'](pointer.fromAddr(void, 0)), pointer.fromAddr(void, 0))

-- void return and no args.
lib['testVoid']()

-- bool arg and bool return.
t.eq(lib['testBool'](true), true)
t.eq(lib['testBool'](false), false)

-- Callback.
ffi = os.newFFI()
ffi.cfunc('testCallback', .{symbol.int, symbol.int, symbol.funcPtr}, symbol.int)
var add = func(a int, b int) int:
    return a + b
lib = ffi.bindLib(libPath)
var cadd = ffi.bindCallback(add, .{symbol.int, symbol.int}, symbol.int)
t.eq(lib['testCallback'](10, 20, cadd.ptr()), 30)

-- bindObjPtr.
ffi = os.newFFI()
var obj = {a=123}
var objPtr = ffi.bindObjPtr(obj)
var obj2 = objPtr.asObject()
-- t.eq(obj, obj2 as ^Table)

--cytest: pass