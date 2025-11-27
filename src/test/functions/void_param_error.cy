fn foo(a void):
    pass

--cytest: error
--CompileError: `void` can not be used as common type specifier.
--
--@MainPath():1:10:
--fn foo(a void):
--         ^
--