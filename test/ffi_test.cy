-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'
import os 'os'

-- Not found.
lib = os.bindLib('xyz123.so', [])
try t.eq(lib, error(#FileNotFound))

if os.system == 'macos':
  -- rdynamic doesn't work atm for MacOS.
  libPath = 'test/macos_lib.dylib'
else os.system == 'windows':
  libPath = 'test/win_lib.dll'
else:
  libPath = none

-- Missing symbol.
lib = os.bindLib(libPath, [ os.CFunc{ sym: 'missing123', args: [], ret: #int }])
try t.eq(lib, error(#MissingSymbol))

object MyObject:
  a number
  b number
  c string
  d boolean

lib = try os.bindLib(libPath, [
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
  os.CFunc{ sym: 'testCharPtrZ', args: [#charPtrZ], ret: #charPtrZ }
  os.CFunc{ sym: 'testDupeCharPtrZ', args: [#dupeCharPtrZ], ret: #charPtrZ }
  os.CFunc{ sym: 'testPtr', args: [#voidPtr], ret: #voidPtr }
  os.CFunc{ sym: 'testVoid', args: [], ret: #void }
  os.CFunc{ sym: 'testBool', args: [#bool], ret: #bool }
  os.CFunc{ sym: 'testObject', args: [MyObject], ret: MyObject }
  os.CFunc{ sym: 'testRetObjectPtr', args: [MyObject], ret: #voidPtr }
  os.CStruct{ fields: [#double, #int, #charPtrZ, #bool], type: MyObject }
])
try t.eq(lib.testAdd(123, 321), 444)
try t.eq(lib.testI8(-128), -128)
try t.eq(lib.testU8(255), 255)
try t.eq(lib.testI16(-32768), -32768)
try t.eq(lib.testU16(65535), 65535)
try t.eq(lib.testI32(-2147483648), -2147483648)
try t.eq(lib.testU32(4294967295), 4294967295)
try t.eq(lib.testI64(-123456789000), -123456789000)
try t.eq(lib.testU64(123456789000), 123456789000)
try t.eq(lib.testUSize(123456789000), 123456789000)
try t.eqNear(lib.testF32(1.2345), 1.2345)
try t.eq(lib.testF64(1.2345), 1.2345)

-- object arg and return type.
res = lib.testObject(MyObject{ a: 123, b: 10, c: 'foo', d: true})
try t.eq(res.a, 123)
try t.eq(res.b, 10)
try t.eq(res.c, rawstring('foo'))
try t.eq(res.d, true)

-- Return struct ptr and convert to Cyber object.
ptr = lib.testRetObjectPtr(MyObject{ a: 123, b: 10, c: 'foo', d: true})
try t.eq(valtag(ptr), #pointer)
res = lib.ptrToMyObject(pointer(ptr))
try t.eq(res.a, 123)
try t.eq(res.b, 10)
try t.eq(res.c, rawstring('foo'))
try t.eq(res.d, true)

-- testCharPtrZ with const string
try t.eq(lib.testCharPtrZ('foo'), rawstring('foo'))

-- testCharPtrZ with heap string
str = 'foo{123}'
try t.eq(lib.testCharPtrZ(str), rawstring('foo123'))

-- testCharPtrZ with number
try t.eq(lib.testCharPtrZ(100), rawstring('100'))

-- testCharPtrZ with rawstring
try t.eq(lib.testCharPtrZ(rawstring('foo')), rawstring('foo'))

-- testDupeCharPtrZ
try t.eq(lib.testDupeCharPtrZ('foo'), rawstring('foo'))

-- testPtr
try t.eq(lib.testPtr(pointer(123)), pointer(123))

-- void return and no args.
try t.eq(lib.testVoid(), none)

-- bool arg and bool return.
try t.eq(lib.testBool(true), true)
try t.eq(lib.testBool(false), false)

-- bindLib that returns a map of functions.
lib = try os.bindLib(libPath, [
  os.CFunc{ sym: 'testAdd', args: [#int, #int], ret: #int }
], { genMap: true })
testAdd = lib['testAdd']
try t.eq(testAdd(123, 321), 444)

-- Reassign a binded function to a static function.
-- TODO: Use statements once initializer block is done.
var staticLibPath: if os.system == 'macos' then 'test/macos_lib.dylib' else if os.system == 'windows' then 'test/win_lib.dll' else none
var staticLib: os.bindLib(staticLibPath, [
  os.CFunc{ sym: 'testAdd', args: [#int, #int], ret: #int }
], { genMap: true })
func staticAdd(a, b) = staticLib.testAdd
try t.eq(staticAdd(123, 321), 444)
-- Freeing the lib reference should not affect `staticAdd`
staticLib = none
try t.eq(staticAdd(123, 321), 444)