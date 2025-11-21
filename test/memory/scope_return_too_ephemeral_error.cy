borrow := foo(&123)

fn foo(scope a &int) -> scope &int:
    return a

--cytest: error
--CompileError: Cannot assign to local `borrow` with a value that has a shorter lifetime.
--
--main:1:11:
--borrow := foo(&123)
--          ^~~~~~~~~
--