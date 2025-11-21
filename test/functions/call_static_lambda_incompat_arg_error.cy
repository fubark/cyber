fn foo(a int) -> int:
    return a
bar := foo
bar(1.0)

--cytest: error
--CompileError: Expected argument `int`, found `float`, when calling `fn (int) -> int`.
--
--main:4:5:
--bar(1.0)
--    ^~~
--