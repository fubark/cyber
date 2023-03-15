import t 'test'

-- Using single quotes.
a = 'World'
b = 123
try t.eq('Hello {a} {b}', 'Hello World 123')

-- Unescape.
str = '\n\tHello {a} {b}'
try t.eq(str.runeAt(0), 10)
try t.eq(str.runeAt(1), 9)

-- Using double quotes.
try t.eq("Hello {a} {b}", 'Hello World 123')

-- Using triple quotes.
try t.eq('''Hello {a} {b}''', 'Hello World 123')

-- With expr at start.
try t.eq('{10}', '10')

-- With adjacent exprs at start.
try t.eq('{10}{20}', '1020')

-- With nested paren group.
try t.eq('{(1 + 2) * 3}', '9')