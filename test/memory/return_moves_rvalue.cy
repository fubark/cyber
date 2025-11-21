use test
global deinit bool = false

type Foo:
    a int

fn (&Foo) @deinit():
    deinit = true

-- rvalue from init literal.
fn createFoo() -> Foo:
    return Foo{a=123}
a := createFoo()
test.eq(false, deinit)

-- rvalue from function call.
fn createFoo2() -> Foo:
    return createFoo()
b := createFoo2()
test.eq(false, deinit)

--cytest: pass