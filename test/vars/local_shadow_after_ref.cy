use test

-- Shadow captured variable after use.
var a = 1
var foo = fn():
    -- Captured for read.
    test.eq(a, 1)
    -- Declare local `a`.
    var a = 3
    test.eq(a, 3)
foo()

-- Shadow static variable after use.
var .b = 1
var foo2 = fn():
    test.eq(b, 1)
    -- Declare local `b`.
    var b = 3
    test.eq(b, 3)

--cytest: pass