var a = 123
func foo():
    a = 234

--cytest: error
--CompileError: Undeclared variable `a`.
--
--main:3:5:
--    a = 234
--    ^
--