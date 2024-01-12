import t 'test'

-- Single quote literal with escaped single quote.
var str = 'ab\'c'
t.eq(str.len(), 4)
t.eq(str.runeAt(0), `a`)
t.eq(str.runeAt(1), `b`)
t.eq(str.runeAt(2), `'`)
t.eq(str.runeAt(3), `c`)

-- Single quote literal with new line escape sequence.
t.eq('ab\nc', "ab
c")
t.eq('ab\nc'.runeAt(2), 10)
t.eq('abc\n', "abc
")
t.eq('abc\n'.runeAt(3), 10)

-- Alert char.
t.eq('ab\ac'.runeAt(2), 7)

-- Backspace char.
t.eq('ab\bc'.runeAt(2), 8)

-- Escape char.
t.eq('ab\ec'.runeAt(2), 0x1b)

-- Carriage return.
t.eq('ab\rc'.runeAt(2), 13)

-- Tab.
t.eq('ab\tc'.runeAt(2), 9)

-- Escaped backslash.
t.eq('ab\\nc'.runeAt(2), 92)
t.eq('ab\\nc'.runeAt(3), `n`)

--cytest: pass