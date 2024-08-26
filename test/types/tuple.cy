use test

type Foo struct(a int, b String)

var f = Foo{123, 'abc'}
test.eq(f.a, 123)
test.eq(f.b, 'abc')

f = .{234, 'xyz'}
test.eq(f.a, 234)
test.eq(f.b, 'xyz')

f = Foo{a=123, b='abc'}
test.eq(f.a, 123)
test.eq(f.b, 'abc')

f = .{a=234, b='xyz'}
test.eq(f.a, 234)
test.eq(f.b, 'xyz')

--cytest: pass