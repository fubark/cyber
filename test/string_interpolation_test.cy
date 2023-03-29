import t 'test'

-- Using single quotes.
a = 'World'
b = 123
t.eq('Hello {a} {b}', 'Hello World 123')

-- Unescape.
str = '\n\tHello {a} {b}'
t.eq(str.runeAt(0), 10)
t.eq(str.runeAt(1), 9)

-- Using double quotes.
t.eq("Hello {a} {b}", 'Hello World 123')

-- Using triple quotes.
t.eq('''Hello {a} {b}''', 'Hello World 123')

-- With expr at start.
t.eq('{10}', '10')

-- With adjacent exprs at start.
t.eq('{10}{20}', '1020')

-- With nested paren group.
t.eq('{(1 + 2) * 3}', '9')