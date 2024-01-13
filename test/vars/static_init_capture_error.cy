var b = 123
var .a = b

--cytest: error
--CompileError: Could not find the symbol `b`.
--
--main:2:10:
--var .a = b
--         ^
--