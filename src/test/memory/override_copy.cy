use test
global copy bool = false

type Foo:
    a int

fn (&Foo) @copy() -> Foo:
    copy = true
    return Foo{a=$a}

a := Foo{a=123}
test.eq(false, copy)
b := a
test.eq(true, copy)

--cytest: pass