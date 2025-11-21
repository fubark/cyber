use test

-- Initialize void.
a := _
test.eqType(void, type.of(a))
test.eq(_, a)

--cytest: pass