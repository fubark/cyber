use test

fn foo(a int) int:
    return a

type Foo[FN fn(int) int] struct:
    a int

    fn foo(self) int:
        return FN(self.a) + 1

var f = Foo[foo]{a=123}
test.eq(f.foo(), 124)

--cytest: pass