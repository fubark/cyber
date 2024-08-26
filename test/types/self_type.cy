use test

type Foo struct:
    a int

    func foo() Self:
        return .{a=123}

test.eq(Foo.foo().a, 123)

--cytest: pass