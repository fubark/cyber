use test

type Foo[T type, Value T] struct:
    a T

    func get(self) T:
        return self.a + Value

var f = Foo[int, 10]{a=123}
test.eq(f.get(), 133)

--cytest: pass