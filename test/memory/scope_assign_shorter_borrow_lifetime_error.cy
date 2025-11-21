b := foo(&123)

fn foo(scope a &int) -> scope &int:
    return a

--cytest: error
--CompileError: Cannot assign to local `b` with a value that has a shorter lifetime.
--
--main:1:6:
--b := foo(&123)
--     ^~~~~~~~~
--