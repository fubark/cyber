import t 'test'

-- Single quote literal with escaped single quote.
str = 'ab\'c'
try t.eq(str.len(), 4)
try t.eq(str.codeAt(0), asciiCode('a'))
try t.eq(str.codeAt(1), asciiCode('b'))
try t.eq(str.codeAt(2), asciiCode("'"))
try t.eq(str.codeAt(3), asciiCode('c'))

-- Single quote literal with new line escape sequence.
try t.eq('ab\nc', "ab
c")
try t.eq('ab\nc'.codeAt(2), 10)
try t.eq('abc\n', "abc
")
try t.eq('abc\n'.codeAt(3), 10)

-- Alert char.
try t.eq('ab\ac'.codeAt(2), 7)

-- Backspace char.
try t.eq('ab\bc'.codeAt(2), 8)

-- Escape char.
try t.eq('ab\ec'.codeAt(2), 0x1b)

-- Carriage return.
try t.eq('ab\rc'.codeAt(2), 13)

-- Tab.
try t.eq('ab\tc'.codeAt(2), 9)

-- Escaped backslash.
try t.eq('ab\\nc'.codeAt(2), 92)
try t.eq('ab\\nc'.codeAt(3), asciiCode('n'))