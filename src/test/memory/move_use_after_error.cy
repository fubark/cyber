a := 123
b := move a
a

--cytest: error
--CompileError: `a` is no longer alive in this scope.
--
--@MainPath():3:1:
--a
--^
--