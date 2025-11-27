b := foo()

fn foo(a &int) -> scope &int:
    return a

--cytest: error
--CompileError: Missing `scope` parameter.
--
--@MainPath():3:25:
--fn foo(a &int) -> scope &int:
--                        ^~~~
--