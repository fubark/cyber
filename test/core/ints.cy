use t 'test'

-- Once a numeric literal is assigned to untyped local, it becomes an integer.
var a = 10
t.eq(a < 4, false)

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

-- fmt()
t.eq(122.fmt(.b), '1111010')
t.eq(122.fmt(.o), '172')
t.eq(122.fmt(.d), '122')
t.eq(122.fmt(.x), '7a')
t.eq(122.fmt(.c), 'z')
t.eq(2.fmt(.b, {pad: `0`, width: 8}), '00000010')
t.eq(8.fmt(.o, {pad: `0`, width: 3}), '010')
t.eq(10.fmt(.d, {pad: `0`, width: 4}), '0010')
t.eq(16.fmt(.x, {pad: `0`, width: 4}), '0010')

--cytest: pass