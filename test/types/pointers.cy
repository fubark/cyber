use test

-- Read dereferenced `int`.
var a = 123
var b = *a
test.eq(b.*, 123)

-- pointer indexing, primitive.
test.eq(b[0], 123)

-- pointer set index, primitive.
b[0] = 234
test.eq(b[0], 234)
test.eq(a, 234)

-- pointer slicing
test.eq(b[0..10].len(), 10)

-- Write to `int` pointer.
b.* = 345
test.eq(b.*, 345)
test.eq(a, 345)

type Foo cstruct:
    a int

-- Read dereferenced cstruct.
var f = Foo{a=123}
var g = *f
test.eq(g.a, 123)
test.eq(g.*.a, 123)

-- pointer indexing, cstruct.
test.eq(g[0].a, 123)

-- pointer set index, cstruct.
g[0] = Foo{a=234}
test.eq(g[0].a, 234)
test.eq(f.a, 234)

-- Write to cstruct pointer.
g.* = Foo{a=345}
test.eq(g.a, 345)

-- Write to cstruct pointer member.
g.a = 456
test.eq(g.a, 456)

-- pointer.fromAddr
var ptr = pointer.fromAddr(void, 0xDEADBEEF)
test.eq(ptr.addr(), 3735928559)

-- pointer.fromRef
var a2 = 123
var a2_ref = ^a2
var a2_ptr = pointer.fromRef(a2_ref)
test.eq(a2_ptr.*, 123)

-- Infer pointer child type.
var foo_ptr *Foo = *.{a=123}
test.eq(foo_ptr.a, 123)

-- Infer pointer slice child type.
var foo_slice [*]Foo = *.{
    .{a=123},
    .{a=234},
}
test.eq(foo_slice.len(), 2)
test.eq(foo_slice[0].a, 123)
test.eq(foo_slice[1].a, 234)

--cytest: pass