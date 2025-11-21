a := 1
a := 2

--cytest: error
--CompileError: Variable `a` is already declared in the block.
--
--main:2:1:
--a := 2
--^
--