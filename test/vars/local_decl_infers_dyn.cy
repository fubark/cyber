use test

dyn a = 123

var b = a
test.eq(b + 1, 124)

--cytest: pass