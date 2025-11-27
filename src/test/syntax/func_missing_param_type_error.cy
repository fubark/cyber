fn foo(a):
    pass

--cytest: error
--CompileError: Expected parameter type.
--
--@MainPath():1:8:
--fn foo(a):
--       ^
--