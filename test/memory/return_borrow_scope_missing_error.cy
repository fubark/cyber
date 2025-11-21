b := foo()

fn foo(a &int) -> &int:
    return a

--cytest: error
--CompileError: Returning a borrow requires a `scope` modifier.
--
--main:3:19:
--fn foo(a &int) -> &int:
--                  ^~~~
--