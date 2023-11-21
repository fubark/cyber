-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'
import os

-- Not found.
var ffi = os.newFFI()
my lib = try ffi.bindLib('xyz123.so')
t.eq(lib, error.FileNotFound)

my libPath = none
if os.system == 'macos':
    -- rdynamic doesn't work atm for MacOS.
    libPath = 'test/macos_lib.dylib'
else os.system == 'windows':
    libPath = 'test/win_lib.dll'
else:
    libPath = none

-- Missing symbol.
ffi = os.newFFI()
ffi.cfunc('missing123', [], .int)
lib = try ffi.bindLib(libPath)
t.eq(lib, error.MissingSymbol)

type MyObject object:
    var a float
    var b int
    var c pointer
    var d bool

ffi = os.newFFI()
ffi.cbind(MyObject, [.double, .int, .charPtr, .bool])
ffi.cfunc('testAdd', [.int, .int], .int)
ffi.cfunc('testI8', [.char], .char)
ffi.cfunc('testU8', [.uchar], .uchar)
ffi.cfunc('testI16', [.short], .short)
ffi.cfunc('testU16', [.ushort], .ushort)
ffi.cfunc('testI32', [.int], .int)
ffi.cfunc('testU32', [.uint], .uint)
ffi.cfunc('testI64', [.long], .long)
ffi.cfunc('testU64', [.ulong], .ulong)
ffi.cfunc('testUSize', [.usize], .usize)
ffi.cfunc('testF32', [.float], .float)
ffi.cfunc('testF64', [.double], .double)
ffi.cfunc('testCharPtr', [.charPtr], .charPtr)
ffi.cfunc('testVoidPtr', [.voidPtr], .voidPtr)
ffi.cfunc('testVoid', [], .void)
ffi.cfunc('testBool', [.bool], .bool)
ffi.cfunc('testObject', [MyObject], MyObject)
ffi.cfunc('testRetObjectPtr', [MyObject], .voidPtr)
ffi.cfunc('testArray', [[os.CArray n: 2, elem: .double]], .double)
lib = ffi.bindLib(libPath)

t.eq(lib.testAdd(123, 321), 444)
t.eq(lib.testI8(-128), -128)
t.eq(lib.testU8(255), 255)
t.eq(lib.testI16(-32768), -32768)
t.eq(lib.testU16(65535), 65535)
t.eq(lib.testI32(-2147483648), -2147483648)
t.eq(lib.testU32(4294967295), 4294967295)
t.eq(lib.testI64(-123456789000), -123456789000)
t.eq(lib.testU64(123456789000), 123456789000)
t.eq(lib.testUSize(123456789000), 123456789000)
t.eqNear(lib.testF32(1.2345), 1.2345)
t.eq(lib.testF64(1.2345), 1.2345)
t.eq(lib.testArray([123.0, 321.0]), 444.0)

-- object arg and return type.
var cstr = os.cstr('foo')
my res = lib.testObject([MyObject a: 123.0, b: 10, c: cstr, d: true])
t.eq(res.a, 123.0)
t.eq(res.b, 10)
t.eq(os.fromCstr(res.c as pointer), array('foo'))
t.eq(res.d, true)
os.free(cstr)

-- Return struct ptr and convert to Cyber object.
cstr = os.cstr('foo')
var ptr = lib.testRetObjectPtr([MyObject a: 123.0, b: 10, c: cstr, d: true])
t.eq(typesym(ptr), .pointer)
res = lib.ptrToMyObject(pointer(ptr))
t.eq(res.a, 123.0)
t.eq(res.b, 10)
t.eq(os.fromCstr(res.c as pointer), array('foo'))
t.eq(res.d, true)
os.free(cstr)

-- testCharPtr
cstr = os.cstr('foo')
t.eq(os.fromCstr(lib.testCharPtr(cstr) as pointer), array('foo'))
os.free(cstr)

-- testVoidPtr
t.eq(lib.testVoidPtr(pointer(123)), pointer(123))
t.eq(lib.testVoidPtr(none), pointer(0))

-- void return and no args.
t.eq(lib.testVoid(), none)

-- bool arg and bool return.
t.eq(lib.testBool(true), true)
t.eq(lib.testBool(false), false)

-- bindLib that returns a map of functions.
ffi = os.newFFI()
ffi.cfunc('testAdd', [.int, .int], .int)
lib = ffi.bindLib(libPath, [genMap: true])
var testAdd = lib['testAdd']
t.eq(testAdd(123, 321), 444)

-- Reassign a binded function to a static function.
-- TODO: Use statements once initializer block is done.
ffi = os.newFFI()
ffi.cfunc('testAdd', [.int, .int], .int)
lib = ffi.bindLib(libPath, [genMap: true])
staticAdd = lib.testAdd
func staticAdd(a int, b int) int:
    pass

t.eq(staticAdd(123, 321), 444)
-- Freeing the lib reference should not affect `staticAdd`
lib = none
t.eq(staticAdd(123, 321), 444)

-- Callback.
ffi = os.newFFI()
ffi.cfunc('testCallback', [.int, .int, .funcPtr], .int)
var add = func(a, b):
    return a + b
lib = ffi.bindLib(libPath)
var cadd = ffi.addCallback(add, [.int, .int], .int)
t.eq(lib.testCallback(10, 20, cadd), 30)

-- addObjPtr.
ffi = os.newFFI()
var obj = [a: 123]
var objPtr = ffi.addObjPtr(obj)
var obj2 = objPtr.asObject()
t.eq(obj, obj2)