use t 'test'

t.eq(a, 123)

--cytest: error
--CompileError: Undeclared variable `a`.
--
--@MainPath():3:6:
--t.eq(a, 123)
--     ^
--