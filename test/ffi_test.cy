import t 'test'

-- Not found.
lib = bindLib('xyz123.so', [])
try t.eq(lib, error(#FileNotFound))

-- Missing symbol.
lib = bindLib(none, [ CFunc{ sym: 'missing123', args: [], ret: #int }])
try t.eq(lib, error(#MissingSymbol))

lib = bindLib(none, [
  CFunc{ sym: 'testAdd', args: [#int, #int], ret: #int }
  CFunc{ sym: 'testI8', args: [#i8], ret: #i8 }
  CFunc{ sym: 'testU8', args: [#u8], ret: #u8 }
  CFunc{ sym: 'testI16', args: [#i16], ret: #i16 }
  CFunc{ sym: 'testU16', args: [#u16], ret: #u16 }
  CFunc{ sym: 'testI32', args: [#i32], ret: #i32 }
  CFunc{ sym: 'testU32', args: [#u32], ret: #u32 }
  CFunc{ sym: 'testF32', args: [#f32], ret: #f32 }
  CFunc{ sym: 'testF64', args: [#f64], ret: #f64 }
  CFunc{ sym: 'testCharPtrZ', args: [#charPtrZ], ret: #charPtrZ }
  CFunc{ sym: 'testPtr', args: [#ptr], ret: #ptr }
  CFunc{ sym: 'testVoid', args: [], ret: #void }
  CFunc{ sym: 'testBool', args: [#bool], ret: #bool }
])
try t.eq(lib.testAdd(123, 321), 444)
try t.eq(lib.testI8(-128), -128)
try t.eq(lib.testU8(255), 255)
try t.eq(lib.testI16(-32768), -32768)
try t.eq(lib.testU16(65535), 65535)
try t.eq(lib.testI32(-2147483648), -2147483648)
try t.eq(lib.testU32(4294967295), 4294967295)
try t.eqNear(lib.testF32(1.2345), 1.2345)
try t.eq(lib.testF64(1.2345), 1.2345)
-- pass in const string
try t.eq(lib.testCharPtrZ('foo'), rawstring('foo'))
-- pass in heap string
str = 'foo{123}'
try t.eq(lib.testCharPtrZ(str), rawstring('foo123'))
try t.eq(lib.testPtr(opaque(123)), opaque(123))

-- void return and no args.
try t.eq(lib.testVoid(), none)

-- bool arg and bool return.
try t.eq(lib.testBool(true), true)
try t.eq(lib.testBool(false), false)