c := 123
b := foo(&c)

fn foo(scope c &int) -> scope &int:
    a := 123
    return &a

--cytest: error
--CompileError: Expected to return a borrow from the same scope as `c`.
--
--main:6:5:
--    return &a
--    ^~~~~~~~~
--