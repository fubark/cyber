fn foo(a):
    pass

--cytest: error
--CompileError: Expected parameter type.
--
--main:1:8:
--fn foo(a):
--       ^
--