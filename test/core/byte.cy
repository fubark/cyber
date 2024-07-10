use t 'test'

-- Initialized with `byte` function.
var a = byte(10)
t.eq(a, 10)

-- Hex notation.
t.eq(byte(0xff), 255)
t.eq(byte(0xFF), 255)

-- Octal notation.
t.eq(byte(0o77), 63)

-- Binary notation.
t.eq(byte(0b11), 3)

-- Equals.
t.eq(byte(3) == byte(2), false)
t.eq(byte(3) == byte(3), true)

-- Not equals.
t.eq(byte(3) != byte(2), true)
t.eq(byte(3) != byte(3), false)

-- Comparisons.
t.eq(byte(1) < 2, true)
t.eq(byte(2) < 1, false)
t.eq(byte(1) > 2, false)
t.eq(byte(2) > 1, true)
t.eq(byte(1) <= 2, true)
t.eq(byte(2) <= 2, true)
t.eq(byte(3) <= 2, false)
t.eq(byte(1) >= 2, false)
t.eq(byte(2) >= 2, true)
t.eq(byte(3) >= 2, true)

-- Bitwise and.
t.eq(byte(4) & 2, 0)
t.eq(byte(4) & 4, 4)
t.eq(byte(7) & 2, 2)

-- Bitwise or.
t.eq(byte(4) | 2, 6)
t.eq(byte(4) | 4, 4)

-- Bitwise xor.
t.eq(byte(4) || 2, 6)
t.eq(byte(4) || 4, 0)

-- Bitwise not.
t.eq(~byte(0), 255)
t.eq(~byte(1), 254)

-- Bitwise right shift.
t.eq(byte(16) >> 2, 4)
t.eq(byte(2) >> 2, 0)

-- Bitwise left shift.
t.eq(byte(2) << 4, 32)

-- fmt()
t.eq(byte(122).fmt(.bin), '1111010')
t.eq(byte(122).fmt(.oct), '172')
t.eq(byte(122).fmt(.dec), '122')
t.eq(byte(122).fmt(.hex), '7a')
t.eq(byte(122).fmt(.asc), 'z')
t.eq(byte(2).fmt(.bin, {pad=`0`, width=8}), '00000010')
t.eq(byte(8).fmt(.oct, {pad=`0`, width=3}), '010')
t.eq(byte(10).fmt(.dec, {pad=`0`, width=4}), '0010')
t.eq(byte(16).fmt(.hex, {pad=`0`, width=4}), '0010')

--cytest: pass