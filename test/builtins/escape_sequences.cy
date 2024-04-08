use t 'test'

-- Double quote literal with escaped double quote.
var str = "ab\"c"
t.eq(str.len(), 4)
t.eq(str[0], `a`)
t.eq(str[1], `b`)
t.eq(str[2], `"`)
t.eq(str[3], `c`)

-- Single quote literal with new line escape sequence.
t.eq("ab\nc", """ab
c""")
t.eq("ab\nc"[2], 10)
t.eq("abc\n", """abc
""")
t.eq("abc\n"[3], 10)

-- Alert char.
t.eq("ab\ac"[2], 7)

-- Backspace char.
t.eq("ab\bc"[2], 8)

-- Escape char.
t.eq("ab\ec"[2], 0x1b)

-- Carriage return.
t.eq("ab\rc"[2], 13)

-- Tab.
t.eq("ab\tc"[2], 9)

-- Escaped backslash.
t.eq("ab\\nc"[2], 0x5c)
t.eq("ab\\nc"[3], `n`)

-- Hex.
t.eq("\x61"[0], `a`)

--cytest: pass