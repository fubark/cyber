use test

type Foo cstruct:
    val int

-- Initialize value.
a := Foo{val=123}
test.eq(a.val, 123)

-- Initialize with inferred type.
var a2 Foo = {val=234}
test.eq(a2.val, 234)

-- C structs do not have required fields. They are zeroed.
a = Foo{}
test.eq(0, a.val)

-- Set field.
a.val = 234
test.eq(a.val, 234)

-- Assignment copies value.
b := a
a.val = 123
test.eq(b.val, 234)
test.eq(a.val, 123)

-- Pass call argument by value.
-- Original value remains unchanged.
fn foo(a Foo):
    test.eq(a.val, 123)
    a.val = 234
a.val = 123
foo(a)
test.eq(a.val, 123)

-- Nested cstruct.
type Bar cstruct:
    a int
    b Foo
c := Bar{a=123, b={val=234}}
test.eq(c.a, 123)
test.eq(c.b.val, 234)
c2 := c
cb := c.b
c.a = 1234
test.eq(c.a, 1234)
c.b.val = 2345
test.eq(c.b.val, 2345)
test.eq(c2.a, 123)
test.eq(c2.b.val, 234)
test.eq(cb.val, 234)

--cytest: pass