use test

-- Shadow captured variable after use.
a := ^1
foo := fn():
    -- Captured for read.
    test.eq(a.*, 1)
    -- Declare local `a`.
    a := 3
    test.eq(a, 3)
foo()

--cytest: error
--CompileError: Variable `a` is a captured variable in the function.
--
--@MainPath():9:5:
--    a := 3
--    ^
--