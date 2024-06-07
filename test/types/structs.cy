use test

type Foo struct:
    val int

-- Initialize value.
var a = Foo{val=123}
test.eq(a.val, 123)

-- Initalize with inferred type.
var a2 Foo = {val=123}
test.eq(a2.val, 123)

-- Set field.
a.val = 234
test.eq(a.val, 234)

-- Assignment copies value.
var b = a
a.val = 123
test.eq(b.val, 234)
test.eq(a.val, 123)

-- Pass call argument by value.
-- Original value remains unchanged.
func foo(a Foo):
    test.eq(a.val, 123)
    a.val = 234
a.val = 123
foo(a)
test.eq(a.val, 123)

-- Nested struct.
type Bar struct:
    a int
    b Foo
var c = Bar{a=123, b = {val=234}}
test.eq(c.a, 123)
test.eq(c.b.val, 234)
var c2 = c
var cb = c.b
c.a = 1234
c.b.val = 2345
test.eq(c.a, 1234)
test.eq(c.b.val, 2345)
test.eq(c2.a, 123)
test.eq(c2.b.val, 234)
test.eq(cb.val, 234)

--cytest: pass