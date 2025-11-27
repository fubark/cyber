if true:
    a := 123
if true:
    a

--cytest: error
--CompileError: Undeclared variable `a`.
--
--@MainPath():4:5:
--    a
--    ^
--