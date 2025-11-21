use test
global deinit bool = false

type Foo:
    a int

fn (&Foo) @deinit():
    deinit = true

fn foo() -> void:
    local := Foo{a=123}

test.eq(false, deinit)
foo()
test.eq(true, deinit)

--cytest: pass