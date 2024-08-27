use test

type S struct(a int, b String)

var s = S{123, 'abc'}
test.eq(s.a, 123)
test.eq(s.b, 'abc')

s = .{234, 'xyz'}
test.eq(s.a, 234)
test.eq(s.b, 'xyz')

s = S{a=123, b='abc'}
test.eq(s.a, 123)
test.eq(s.b, 'abc')

s = .{a=234, b='xyz'}
test.eq(s.a, 234)
test.eq(s.b, 'xyz')

type O(a int, b String)

var o = O{123, 'abc'}
test.eq(o.a, 123)
test.eq(o.b, 'abc')

o = .{234, 'xyz'}
test.eq(o.a, 234)
test.eq(o.b, 'xyz')

o = O{a=123, b='abc'}
test.eq(o.a, 123)
test.eq(o.b, 'abc')

o = .{a=234, b='xyz'}
test.eq(o.a, 234)
test.eq(o.b, 'xyz')

--cytest: pass