use test
global deinit bool = false

type Foo:
    a int

fn (&Foo) @deinit():
    deinit = true

-- rvalue from init literal.
a := ^Foo{a=123}
test.eq(false, deinit)

-- rvalue from function call.
fn createFoo() -> Foo:
    return Foo{a=123}
b := ^createFoo()
test.eq(false, deinit)

--cytest: pass