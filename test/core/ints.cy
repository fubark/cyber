use t 'test'

-- Once a numeric literal is assigned to untyped local, it becomes an integer.
var a = 10
t.eq(a, 10)

-- Hex notation.
t.eq(0xff, 255)
t.eq(0xFF, 255)

-- Octal notation.
t.eq(0o77, 63)

-- Binary notation.
t.eq(0b11, 3)

-- UTF-8 Rune notation.
t.eq(`a`, 97)
t.eq(`A`, 65)
t.eq(`'`, 39)    -- Escape single quote.
t.eq(`\\`, 92)    -- Escape backslash.
t.eq(`🐶`, 128054)
t.eq(`🦊`, 129418)
t.eq(`\n`, 10)

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
t.eq(4 & 2, 0)
t.eq(4 & 4, 4)
t.eq(7 & 2, 2)

-- Bitwise or.
t.eq(4 | 2, 6)
t.eq(4 | 4, 4)

-- Bitwise xor.
t.eq(4 || 2, 6)
t.eq(4 || 4, 0)

-- Bitwise not.
t.eq(~0, -1)
t.eq(~(-1), 0)
t.eq(~1, -2)

-- Bitwise right shift.
t.eq(16 >> 2, 4)
t.eq(2 >> 2, 0)
t.eq(-1 >> 1, -1)  -- Performs sign extension
t.eq(-2147483648 >> 1, -1073741824)
t.eq((-140737488355327 - 1) >> 48, -1)

-- Bitwise left shift.
t.eq(2 << 4, 32)
t.eq(1 << 63, (-9223372036854775807-1))
t.eq((-9223372036854775807-1) << 1, 0)  

-- fmt()
t.eq(122.fmt(.bin), '1111010')
t.eq(122.fmt(.oct), '172')
t.eq(122.fmt(.dec), '122')
t.eq(122.fmt(.hex), '7a')
t.eq(122.fmt(.asc), 'z')
t.eq(2.fmt(.bin, {pad=`0`, width=8}), '00000010')
t.eq(8.fmt(.oct, {pad=`0`, width=3}), '010')
t.eq(10.fmt(.dec, {pad=`0`, width=4}), '0010')
t.eq(16.fmt(.hex, {pad=`0`, width=4}), '0010')

--cytest: pass