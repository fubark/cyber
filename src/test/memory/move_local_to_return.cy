use test
global deinit bool = false

type Foo:
    a int

fn (&Foo) @deinit():
    deinit = true

fn createFoo() -> Foo:
    local := Foo{a=123}
    return move local
a := createFoo()
test.eq(false, deinit)

--cytest: pass