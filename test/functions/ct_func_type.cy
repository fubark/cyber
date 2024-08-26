use test

func foo(a int) int:
    return a

type Foo[FN func(int) int] struct:
    a int

    func foo(self) int:
        return FN(self.a) + 1

var f = Foo[foo]{a=123}
test.eq(f.foo(), 124)

--cytest: pass