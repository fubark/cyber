b := 123
global a int = b

--cytest: error
--CompileError: Undeclared variable `b`.
--
--main:2:16:
--global a int = b
--               ^
--