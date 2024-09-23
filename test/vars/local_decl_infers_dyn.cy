use test

dyn a = 123

var b = a
test.eq(1 + b, 124)

--cytest: pass