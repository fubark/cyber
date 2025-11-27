use test

fn foo(a int) -> int:
    return a

type Fn1 = fnsym(int) -> int

type Foo[const FN Fn1] struct:
    a int

fn (&Foo[]) foo() -> int:
    return FN(self.a) + 1

f := Foo[foo]{a=123}
test.eq(f.foo(), 124)

--cytest: pass