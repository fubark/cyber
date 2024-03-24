import test

my a = test.erase(123)

var b = a
test.eq(b + 1, 124)

--cytest: pass