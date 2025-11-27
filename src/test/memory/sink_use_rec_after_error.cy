use test

s := S{a=123}
test.eq(123, s.foo())
s.foo()

type S:
    a int

fn (sink S) foo() -> int:
    return @a

--cytest: error
--CompileError: `s` is no longer alive in this scope.
--
--@MainPath():5:1:
--s.foo()
--^
--