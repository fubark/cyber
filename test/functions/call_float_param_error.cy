fn foo(a float):
    pass
foo(true)

--cytest: error
--CompileError: Expected argument `float`, found `bool`, when calling `fn foo(float) -> void`.
--
--main:3:5:
--foo(true)
--    ^~~~
--