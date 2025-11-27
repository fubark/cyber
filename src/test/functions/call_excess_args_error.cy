fn foo(a int) -> int:
    return a + 3
foo(1, 2)

--cytest: error
--CompileError: Expected `1` arguments, found `2`, when calling the function `fn foo(int) -> int`.
--
--@MainPath():3:1:
--foo(1, 2)
--^~~~~~~~~
--