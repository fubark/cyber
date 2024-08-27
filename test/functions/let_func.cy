use test

-- No params
let foo():
    return 123
test.eq(foo(), 123)

-- Params.
let foo2(a, b):
    return a + b
test.eq(foo2(1, 2), 3)

--cytest: pass