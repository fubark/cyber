fn foo(a bool):
    pass
foo(123)

--cytest: error
--CompileError: Expected argument `bool`, found `int`, when calling `fn foo(bool) -> void`.
--
--main:3:5:
--foo(123)
--    ^~~
--