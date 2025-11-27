fn foo(a bool):
    pass
foo(123)

--cytest: error
--CompileError: Expected argument `bool`, found `int`, when calling `fn foo(bool) -> void`.
--
--@MainPath():3:5:
--foo(123)
--    ^~~
--