use test

-- Wrap some.
a := ?int(123)
test.eq(a == none, false)
test.eq(a != none, true)
test.eq(a.?, 123)

-- Infer target.
a = 234
test.eq(a.?, 234)

-- Equivalent to ?int. 
b := Option[int](123)
test.eq(b.?, 123)

-- Wrap variable.
ca := 123
c := ?int(ca)
test.eq(c.?, 123)

-- Wrap struct.
type Foo:
    a int
d := ?Foo({a=123})
test.eq(d.?.a, 123)

-- Wrap none, implicitly.
b = none
test.eq(b == none, true)
test.eq(b != none, false)

-- Wrap none, explicitly.
b2 := ?int(none)
test.assert(b2 == none)

-- ?else unwraps value.
b = 234
test.eq(b ?else 123, 234)

-- ?else defaults.
b = none
test.eq(b ?else 123, 123)

-- ?else block unwraps value.
b = 234
b_val := b ?else:
    panic('Unexpected')
test.eq(234, b_val)

-- ?else block branches to none case.
fn foo() -> int:
    res := ?int(none)
    return res ?else:
        return 0
test.eq(0, foo())

--cytest: pass