use test

type Foo struct:
    a int

    -- Test using `Self` in function signature.
    func foo() Self:
        return .{a=123}

    -- Test using `Self` inside function body.
    func foo2() Self:
        var new = Self{a=123}
        return new

test.eq(Foo.foo().a, 123)
test.eq(Foo.foo2().a, 123)

--cytest: pass