fn foo() -> int:
    return 1

fn foo(n int) -> int:
    return n

foo(1, 2)

--cytest: error
--CompileError: Expected `0` arguments, found `2`, when calling the function `fn foo() -> int`.
--
--main:7:1:
--foo(1, 2)
--^~~~~~~~~
--