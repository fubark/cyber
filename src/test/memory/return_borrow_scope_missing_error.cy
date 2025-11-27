b := foo()

fn foo(a &int) -> &int:
    return a

--cytest: error
--CompileError: Returning a borrow requires a `scope` modifier.
--
--@MainPath():3:19:
--fn foo(a &int) -> &int:
--                  ^~~~
--