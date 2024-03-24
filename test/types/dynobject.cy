import test

type Foo dynobject:
    a int

-- Initialize with missing field.
var f = Foo{a: 123, b: 234}
test.eq(f.a, 123)
test.eq(f.b, 234)

-- Setting missing field.
f.c = 345
test.eq(f.c, 345)

-- Test bc small allocation.
type FooSmall dynobject
var s = FooSmall{a: 123, b: 234}
test.eq(s.a, 123)
test.eq(s.b, 234)
s.c = 345
test.eq(s.c, 345)

--cytest: pass