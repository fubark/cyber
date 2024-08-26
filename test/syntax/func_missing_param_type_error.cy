func foo(a):
    pass

--cytest: error
--CompileError: Expected parameter type.
--
--main:1:10:
--func foo(a):
--         ^
--