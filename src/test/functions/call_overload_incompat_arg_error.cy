fn foo(a int):
    pass

fn foo(a bool):
    pass

arg := 1.23
foo(arg)

--cytest: error
--CompileError: Expected argument `int`, found `float`, when calling `fn foo(int) -> void`.
--
--@MainPath():8:5:
--foo(arg)
--    ^~~
--