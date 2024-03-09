import t 'test'

var escaped = 'Return the underlying `symbol`.'.replace('`', '\\`')
t.eq(escaped, 'Return the underlying \\`symbol\\`.')

-- Single quotes does not interpret escape characters.
var a = '\n'
t.eq(a.len(), 2)
a[0] == `\\`
a[1] == `n`

-- Indexing an invalid utf8.
var invalid = "\xc3\x28"
t.eq(invalid.len(), 2)
t.eq(invalid.count(), 1)
t.eq(invalid[0], 0xFFFD)
-- var iter = invalid.iterator()
-- t.eq(iter.next(), 0xFFFD)

--cytest: pass