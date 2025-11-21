foo = 1

--cytest: error
--CompileError: Undeclared variable `foo`.
--
--main:1:1:
--foo = 1
--^~~
--