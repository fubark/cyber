b := foo()

fn foo(scope a &int, scope b &int) -> scope &int:
    return a

--cytest: error
--CompileError: Expected only one parameter with the `scope` modifier.
--
--main:3:28:
--fn foo(scope a &int, scope b &int) -> scope &int:
--                           ^
--