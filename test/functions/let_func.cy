import test

-- No params
let foo():
    return 123
test.eq(foo(), 123)

-- Params.
let foo2(a, b):
    return a + b
test.eq(foo2(1, 2), 3)

-- Lambda.
let foo3 = (a, b):
    return a + b
test.eq(foo3(1, 2), 3)

-- Lambda shorthand.
let foo4 = (a, b) => a + b
test.eq(foo4(1, 2), 3)

--cytest: pass