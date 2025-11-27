if true:
    a := 1
a

--cytest: error
--CompileError: Undeclared variable `a`.
--
--@MainPath():3:1:
--a
--^
--