use test

-- Read dereferenced `int`.
var a = 123
var b = &a
test.eq(b.*, 123)

-- pointer indexing
test.eq(b[0], 123)

-- pointer slicing
test.eq(b[0..10].len(), 10)

-- Write to `int` pointer.
b.* = 234
test.eq(b.*, 234)
test.eq(a, 234)

-- Read dereferenced cstruct.
type Foo cstruct:
    a int
var f = Foo{a=123}
var g = &f
test.eq(g.a, 123)
test.eq(g.*.a, 123)

-- Write to cstruct pointer.
g.* = Foo{ a=234 }
test.eq(g.a, 234)

-- Write to cstruct pointer member.
g.a = 345
test.eq(g.a, 345)

-- pointer.$call
var ptr = pointer(void, 0xDEADBEEF)
test.eq(ptr.addr(), 3735928559)

--cytest: pass