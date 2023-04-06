-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'
import os 'os'

-- Not found.
lib = try os.bindLib('xyz123.so', [])
t.eq(lib, error.FileNotFound)

if os.system == 'macos':
  -- rdynamic doesn't work atm for MacOS.
  libPath = 'test/macos_lib.dylib'
else os.system == 'windows':
  libPath = 'test/win_lib.dll'
else:
  libPath = none

-- Missing symbol.
lib = try os.bindLib(libPath, [ os.CFunc{ sym: 'missing123', args: [], ret: #int }])
t.eq(lib, error.MissingSymbol)

type MyObject object:
  a number
  b number
  c string
  d boolean

lib = os.bindLib(libPath, [
  os.CFunc{ sym: 'testAdd', args: [#int, #int], ret: #int }
  os.CFunc{ sym: 'testI8', args: [#char], ret: #char }
  os.CFunc{ sym: 'testU8', args: [#uchar], ret: #uchar }
  os.CFunc{ sym: 'testI16', args: [#short], ret: #short }
  os.CFunc{ sym: 'testU16', args: [#ushort], ret: #ushort }
  os.CFunc{ sym: 'testI32', args: [#int], ret: #int }
  os.CFunc{ sym: 'testU32', args: [#uint], ret: #uint }
  os.CFunc{ sym: 'testI64', args: [#long], ret: #long }
  os.CFunc{ sym: 'testU64', args: [#ulong], ret: #ulong }
  os.CFunc{ sym: 'testUSize', args: [#usize], ret: #usize }
  os.CFunc{ sym: 'testF32', args: [#float], ret: #float }
  os.CFunc{ sym: 'testF64', args: [#double], ret: #double }
  os.CFunc{ sym: 'testCharPtr', args: [#charPtr], ret: #charPtr }
  os.CFunc{ sym: 'testVoidPtr', args: [#voidPtr], ret: #voidPtr }
  os.CFunc{ sym: 'testVoid', args: [], ret: #void }
  os.CFunc{ sym: 'testBool', args: [#bool], ret: #bool }
  os.CFunc{ sym: 'testObject', args: [MyObject], ret: MyObject }
  os.CFunc{ sym: 'testRetObjectPtr', args: [MyObject], ret: #voidPtr }
  os.CStruct{ fields: [#double, #int, #charPtr, #bool], type: MyObject }
])
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

-- object arg and return type.
cstr = os.cstr('foo')
res = lib.testObject(MyObject{ a: 123, b: 10, c: cstr, d: true})
t.eq(res.a, 123)
t.eq(res.b, 10)
t.eq(os.fromCstr(res.c as pointer), rawstring('foo'))
t.eq(res.d, true)
os.free(cstr)

-- Return struct ptr and convert to Cyber object.
cstr = os.cstr('foo')
ptr = lib.testRetObjectPtr(MyObject{ a: 123, b: 10, c: cstr, d: true})
t.eq(typesym(ptr), #pointer)
res = lib.ptrToMyObject(pointer(ptr))
t.eq(res.a, 123)
t.eq(res.b, 10)
t.eq(os.fromCstr(res.c as pointer), rawstring('foo'))
t.eq(res.d, true)
os.free(cstr)

-- testCharPtr
cstr = os.cstr('foo')
t.eq(os.fromCstr(lib.testCharPtr(cstr) as pointer), rawstring('foo'))
os.free(cstr)

-- testVoidPtr
t.eq(lib.testVoidPtr(pointer(123)), pointer(123))

-- void return and no args.
t.eq(lib.testVoid(), none)

-- bool arg and bool return.
t.eq(lib.testBool(true), true)
t.eq(lib.testBool(false), false)

-- bindLib that returns a map of functions.
lib = try os.bindLib(libPath, [
  os.CFunc{ sym: 'testAdd', args: [#int, #int], ret: #int }
], { genMap: true })
testAdd = lib['testAdd']
t.eq(testAdd(123, 321), 444)

-- Reassign a binded function to a static function.
-- TODO: Use statements once initializer block is done.
var staticLibPath: if os.system == 'macos' then 'test/macos_lib.dylib' else if os.system == 'windows' then 'test/win_lib.dll' else none
var staticLib: os.bindLib(staticLibPath, [
  os.CFunc{ sym: 'testAdd', args: [#int, #int], ret: #int }
], { genMap: true })
func staticAdd(a, b) = staticLib.testAdd
t.eq(staticAdd(123, 321), 444)
-- Freeing the lib reference should not affect `staticAdd`
staticLib = none
t.eq(staticAdd(123, 321), 444)