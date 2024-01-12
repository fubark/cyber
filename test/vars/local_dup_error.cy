var a = 1
var a = 2

--cytest: error
--CompileError: Variable `a` is already declared in the block.
--
--main:2:5:
--var a = 2
--    ^
--