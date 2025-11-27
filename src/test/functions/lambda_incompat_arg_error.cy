foo := fn(a int) -> int:
    return a

foo(1.0)

--cytest: error
--CompileError: Expected argument `int`, found `float`, when calling `fn (int) -> int`.
--
--@MainPath():4:5:
--foo(1.0)
--    ^~~
--