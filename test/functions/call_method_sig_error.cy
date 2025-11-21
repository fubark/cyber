type S:
    a int

fn (&S) foo() -> int:
    return 123

o := S{a=123}
o.foo(234)

--cytest: error
--CompileError: Expected `1` arguments, found `2`, when calling the function `fn foo(&S) -> int`.
--
--main:8:1:
--o.foo(234)
--^~~~~~~~~~
--