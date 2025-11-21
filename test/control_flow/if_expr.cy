use test

foo := true
test.eq(if (foo) 123 else 456, 123)
foo = false
test.eq(if (foo) 123 else 456, 456)

-- Heap allocated values.
a := if (false) 'abc' else 'xyz'
test.eq(a, 'xyz')

-- infer from target type.
var b byte = if (false) 12 else 23
test.eq(23, b)

--cytest: pass