global a int = foo()

fn foo() -> int:
    a = 123
    return 123

--cytest: error
--CompileError: Expected pure function.
--
--@MainPath():1:16:
--global a int = foo()
--               ^~~~~
--
