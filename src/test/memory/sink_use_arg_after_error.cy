use test

a := 123
test.eq(123, foo(a))
a

fn foo(sink a int) -> int:
    return a

--cytest: error
--CompileError: `a` is no longer alive in this scope.
--
--@MainPath():5:1:
--a
--^
--