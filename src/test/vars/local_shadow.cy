use test

-- Shadow static variable after use.
global b int = 1
foo2 := fn():
    test.eq(b, 1)
    -- Declare local `b`.
    b := 3
    test.eq(b, 3)

--cytest: pass