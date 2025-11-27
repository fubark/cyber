foo = 1

--cytest: error
--CompileError: Undeclared variable `foo`.
--
--@MainPath():1:1:
--foo = 1
--^~~
--