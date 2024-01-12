var b = 123
var Root.a = b

--cytest: error
--CompileError: Could not find the symbol `b`.
--
--main:2:14:
--var Root.a = b
--             ^
--