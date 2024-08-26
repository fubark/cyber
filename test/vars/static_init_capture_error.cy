var b = 123
var .a = b

--cytest: error
--CompileError: Undeclared variable `b`.
--
--main:2:10:
--var .a = b
--         ^
--