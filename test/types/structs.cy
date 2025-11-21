use test

-- `struct` keyword is optional.
type FooNoKeyword:
    val int

type Foo struct:
    val int

-- Initialize value.
a := Foo{val=123}
test.eq(a.val, 123)

-- Initialize with inferred type.
var a2 Foo = {val=234}
test.eq(a2.val, 234)

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

-- Nested struct.
type Bar struct:
    a int
    b Foo
c := Bar{a=123, b={val=234}}
test.eq(c.a, 123)
test.eq(c.b.val, 234)
c2 := c
cb := c.b
c.a = 1234
c.b.val = 2345
test.eq(c.a, 1234)
test.eq(c.b.val, 2345)
test.eq(c2.a, 123)
test.eq(c2.b.val, 234)
test.eq(cb.val, 234)

-- Multiple structs with the same field names but different offsets.
type Node1:
    a int
    b int
type Node2:
    b int
    a int
na := Node1{a=1, b=2}
nb := Node2{a=3, b=4}
test.eq(na.a, 1)
test.eq(na.b, 2)
test.eq(nb.a, 3)
test.eq(nb.b, 4)

--cytest: pass