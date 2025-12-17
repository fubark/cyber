use t 'test'
use meta

-- @init()
res := int('100')
t.eqType(int, type.of(res))
t.eq(0, int('100.1'))
t.eq(100, res)
t.eq(100, int(100.1))

-- Once a numeric literal is assigned to untyped local, it becomes an integer.
a := 10
t.eq(a, 10)

-- Hex notation.
t.eq(0xff, 255)
t.eq(0xFF, 255)

-- Octal notation.
t.eq(0o77, 63)

-- Binary notation.
t.eq(0b11, 3)

-- Infer UTF-8 code point.
t.eq(97, 'a')
t.eq(65, 'A')
t.eq(39, '\'')    -- Escape single quote.
t.eq(92, '\\')    -- Escape backslash.
t.eq(128054, '🐶')
t.eq(129418, '🦊')
t.eq(10, '\n')

-- Equals.
t.eq(3 == 2, false)
t.eq(3 == 3, true)

-- Not equals.
t.eq(3 != 2, true)
t.eq(3 != 3, false)

-- Comparisons.
t.eq(1 < 2, true)
t.eq(2 < 1, false)
t.eq(1 > 2, false)
t.eq(2 > 1, true)
t.eq(1 <= 2, true)
t.eq(2 <= 2, true)
t.eq(3 <= 2, false)
t.eq(1 >= 2, false)
t.eq(2 >= 2, true)
t.eq(3 >= 2, true)

-- Bitwise and.
t.eq(4 && 2, 0)
t.eq(4 && 4, 4)
t.eq(7 && 2, 2)

-- Bitwise or.
t.eq(4 || 2, 6)
t.eq(4 || 4, 4)

-- Bitwise xor.
t.eq(4 ~ 2, 6)
t.eq(4 ~ 4, 0)

-- Bitwise not.
t.eq(~0, -1)
t.eq(~(-1), 0)
t.eq(~1, -2)

-- Bitwise right shift (logical).
t.eq(4, 16 >> 2)
t.eq(0, 2 >> 2)
t.eq(1, -1 >> 63)
t.eq(0x40000000, 0x80000000 >> 1)
t.eq(1, 0x8000000000000000 >> 63)

use math

-- Bitwise left shift (logical).
t.eq(32, 2 << 4)
t.eq(int.min, 1 << 63)
t.eq(0, int.min << 1)  

-- fmt()
t.eq(122.fmt(.dec), '122')
if meta.is_vm_target():
    t.eq(122.fmt(.bin), '1111010')
    t.eq(122.fmt(.oct), '172')
    t.eq(122.fmt(.hex), '7a')
    t.eq(122.fmt(.ch), 'z')
    -- t.eq(2.fmt(.bin, {pad=`0`, width=8}), '00000010')
    -- t.eq(8.fmt(.oct, {pad=`0`, width=3}), '010')
    -- t.eq(10.fmt(.dec, {pad=`0`, width=4}), '0010')
    -- t.eq(16.fmt(.hex, {pad=`0`, width=4}), '0010')

-- -- Implicit widen.
-- a_int := as[int] i32(-123)
-- t.eq(-123, a_int)

-- Truncate.
a = 123
t.eq(byte(123), byte(a))

-- r64.decode()
vec := [8]byte{0x5a, 0xf1, 0x06, 0x04, 0x5e, 0x23, 0x8d, 0xd2}
t.eq(0x5AF106045E238DD2, r64.decode(&vec, .big))
t.eq(0xD28D235E0406F15A, r64.decode(&vec, .little))

-- r32.decode()
vec2 := [4]byte{0x49, 0x96, 0x02, 0xD2}
t.eq(r32(0x499602D2), r32.decode(&vec2, .big))
t.eq(r32(0xD2029649), r32.decode(&vec2, .little))

--cytest: pass