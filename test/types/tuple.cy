use test

type S struct(a int, b string)

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

-- Struct tuple with functions.
type S2 struct(a int, b string):
    func foo(self):
        pass

type O(a int, b string)

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

-- Object tuple with functions.
type O2(a int, b string):
    func foo(self):
        pass

--cytest: pass