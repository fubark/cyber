import t 'test'

-- Single quote literal with escaped single quote.
str = 'ab\'c'
try t.eq(str.len(), 4)
try t.eq(str.runeAt(0), 0u'a')
try t.eq(str.runeAt(1), 0u'b')
try t.eq(str.runeAt(2), 0u'\'')
try t.eq(str.runeAt(3), 0u'c')

-- Single quote literal with new line escape sequence.
try t.eq('ab\nc', "ab
c")
try t.eq('ab\nc'.runeAt(2), 10)
try t.eq('abc\n', "abc
")
try t.eq('abc\n'.runeAt(3), 10)

-- Alert char.
try t.eq('ab\ac'.runeAt(2), 7)

-- Backspace char.
try t.eq('ab\bc'.runeAt(2), 8)

-- Escape char.
try t.eq('ab\ec'.runeAt(2), 0x1b)

-- Carriage return.
try t.eq('ab\rc'.runeAt(2), 13)

-- Tab.
try t.eq('ab\tc'.runeAt(2), 9)

-- Escaped backslash.
try t.eq('ab\\nc'.runeAt(2), 92)
try t.eq('ab\\nc'.runeAt(3), 0u'n')