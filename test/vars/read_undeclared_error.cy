import t 'test'

t.eq(a, 123)

--cytest: error
--CompileError: Undeclared variable `a`.
--
--main:3:6:
--t.eq(a, 123)
--     ^
--