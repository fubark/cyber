func foo(a void):
    pass

--cytest: error
--CompileError: `void` can not be used as common type specifier.
--
--main:1:12:
--func foo(a void):
--           ^
--