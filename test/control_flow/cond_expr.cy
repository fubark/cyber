import test

var foo = true
test.eq(foo ? 123 else 456, 123)
foo = false
test.eq(foo ? 123 else 456, 456)

-- Types are merged.
var a = false ? 123 else '$(123)456'
test.eq(a, '123456')
-- `a` should be released since else returns a heap string.

--cytest: pass