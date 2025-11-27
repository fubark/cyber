foo()

fn foo() -> int:
    return 123

--cytest: error
--CompileError: Expected assignment to a variable or placeholder for `int`.
--
--@MainPath():1:1:
--foo()
--^~~~~
--