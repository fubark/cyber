if true:
    a := 123
if true:
    a

--cytest: error
--CompileError: Undeclared variable `a`.
--
--main:4:5:
--    a
--    ^
--