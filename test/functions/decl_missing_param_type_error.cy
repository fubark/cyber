func foo(a):
    pass

--cytest: error
--CompileError: Expected type specifier.
--
--main:1:10:
--func foo(a):
--         ^
--