use test

-- Initialize void.
var a = _
test.eq(typeOf(a), void)
test.eq(a, _)

--cytest: pass