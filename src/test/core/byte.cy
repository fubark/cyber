use test
use meta

-- Initialized with `byte` function.
a := byte(10)
test.eq(byte(10), a)

-- Hex notation.
test.eq(byte(255), byte(0xff))
test.eq(byte(255), byte(0xFF))

-- Octal notation.
test.eq(byte(63), byte(0o77))

-- Binary notation.
test.eq(byte(3), byte(0b11))

-- Equals.
test.eq(false, byte(3) == byte(2))
test.eq(true, byte(3) == byte(3))

-- Not equals.
test.eq(true, byte(3) != byte(2))
test.eq(false, byte(3) != byte(3))

-- Comparisons.
test.eq(true, byte(1) < 2)
test.eq(false, byte(2) < 1)
test.eq(false, byte(1) > 2)
test.eq(true, byte(2) > 1)
test.eq(true, byte(1) <= 2)
test.eq(true, byte(2) <= 2)
test.eq(false, byte(3) <= 2)
test.eq(false, byte(1) >= 2)
test.eq(true, byte(2) >= 2)
test.eq(true, byte(3) >= 2)

-- Bitwise and.
test.eq(byte(0), byte(4) && 2)
test.eq(byte(4), byte(4) && 4)
test.eq(byte(2), byte(7) && 2)

-- Bitwise or.
test.eq(byte(6), byte(4) || 2)
test.eq(byte(4), byte(4) || 4)

-- Bitwise xor.
test.eq(byte(6), byte(4) ~ 2)
test.eq(byte(0), byte(4) ~ 4)

-- Bitwise not.
test.eq(byte(255), ~byte(0))
test.eq(byte(254), ~byte(1))

-- Bitwise right shift.
test.eq(byte(4), byte(16) >> 2)
test.eq(byte(0), byte(2) >> 2)

-- Bitwise left shift.
test.eq(byte(32), byte(2) << 4)

-- fmt()
test.eq('122', byte(122).fmt(.dec))
test.eq('0b1111010', byte(122).fmt(.bin))
test.eq('0o172', byte(122).fmt(.oct))
test.eq('0x7a', byte(122).fmt(.hex))
if meta.is_vm_target():
    test.eq('z', byte(122).fmt(.ch))
    -- test.eq('00000010', byte(2).fmt(.bin, {pad=`0`, width=8}))
    -- test.eq('010', byte(8).fmt(.oct, {pad=`0`, width=3}))
    -- test.eq('0010', byte(10).fmt(.dec, {pad=`0`, width=4}))
    -- test.eq('0010', byte(16).fmt(.hex, {pad=`0`, width=4}))

-- Infer from string literal.
var aa byte = 'a'
test.eq(97, aa)

-- Infer optional from string literal.
var opt_aa ?byte = 'a'
test.eq(97, opt_aa.?)

--cytest: pass