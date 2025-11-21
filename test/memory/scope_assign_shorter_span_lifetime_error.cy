b := foo()![..]

fn foo() -> !Buffer[byte]:
    return {}

--cytest: error
--CompileError: Cannot assign to local `b` with a value that has a shorter lifetime.
--
--main:1:6:
--b := foo()![..]
--     ^~~~~~~~~~
--