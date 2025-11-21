b := ^2
foo := fn(a int) -> int:
    return a + b.*
foo(1.0)

--cytest: error
--CompileError: Expected argument `int`, found `float`, when calling `fn (int) -> int`.
--
--main:4:5:
--foo(1.0)
--    ^~~
--