foo()

fn foo() -> int:
    return 123

--cytest: error
--CompileError: Expected assignment to a variable or placeholder for `int`.
--
--main:1:1:
--foo()
--^~~~~
--