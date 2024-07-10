use test

var foo = true
test.eq(if (foo) 123 else 456, 123)
foo = false
test.eq(if (foo) 123 else 456, 456)

-- Heap allocated values.
var a = if (false) 'abc' else 'xyz'
test.eq(a, 'xyz')

-- `any` infer type.
var b any = if (false) 123 else '456'
test.eq(b, '456')

--cytest: pass