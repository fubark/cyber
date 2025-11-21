global a int = foo()

fn foo() -> int:
    a = 123
    return 123

--cytest: error
--CompileError: Expected pure function.
--
--main:1:16:
--global a int = foo()
--               ^~~~~
--
