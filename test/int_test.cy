import t 'test'

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
t.eq(0u'a', 97)
t.eq(0u'A', 65)
t.eq(0u'\'', 39)    -- Escape single quote.
t.eq(0u'\\', 92)    -- Escape backslash.
t.eq(0u'🐶', 128054)
t.eq(0u'🦊', 129418)
t.eq(0u'\n', 10)

