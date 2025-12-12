use test

type Foo[T Any, const Value T]:
    a T

fn (&Foo[]) get() -> T:
    return self.a + #{Value}

f := Foo[int, 10]{a=123}
test.eq(f.get(), 133)

--cytest: pass