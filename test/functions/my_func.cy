import test

-- No params
my foo():
    return 123
test.eq(foo(), 123)

-- Params.
my foo2(a, b):
    return a + b
test.eq(foo2(1, 2), 3)

--cytest: pass