use test

type Foo:
    a int

    -- Test using `Self` in function signature.
    fn foo() Self:
        return .{a=123}

    -- Test using `Self` inside function body.
    fn foo2() Self:
        var new = Self{a=123}
        return new

test.eq(Foo.foo().a, 123)
test.eq(Foo.foo2().a, 123)

--cytest: pass