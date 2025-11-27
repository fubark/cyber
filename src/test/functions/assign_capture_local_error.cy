a := 123
fn foo():
    a = 234

--cytest: error
--CompileError: Undeclared variable `a`.
--
--@MainPath():3:5:
--    a = 234
--    ^
--