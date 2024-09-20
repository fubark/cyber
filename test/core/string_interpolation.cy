use t 'test'

-- Using single quotes.
var a = 'World'
var b = 123
t.eq("Hello ${a} ${b}", 'Hello World 123')

-- Using just dollar sign.
t.eq('$'.len(), 1)
t.eq('$'[0], 36)

-- Escape.
var str = "\n\tHello ${a} ${b}"
t.eq(str[0], 10)
t.eq(str[1], 9)

-- Using string literal.
t.eq("Hello ${a} ${b}", 'Hello World 123')

-- Using multi-line string literal.
t.eq("""Hello ${a} ${b}""", 'Hello World 123')

-- With expr at start.
t.eq("${10}", '10')

-- With adjacent exprs at start.
t.eq("${10}${20}", '1020')

-- With nested paren group.
t.eq("${(1 + 2) * 3}", '9')

-- With nested braces.
t.eq("${{1, 2, 3}[2]}", '3')

---cytest: pass