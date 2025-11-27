for 0..10 |i|:
    pass
i

--cytest: error
--CompileError: Undeclared variable `i`.
--
--@MainPath():3:1:
--i
--^
--