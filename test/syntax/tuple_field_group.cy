use test

type S struct(a, b int)

var s = S{1, 2}
test.eq(s.a, 1)
test.eq(s.b, 2)

type S2 struct(a, b int, c, d string)
var s2 = S2{1, 2, 'abc', 'xyz'}
test.eq(s2.a, 1)
test.eq(s2.b, 2)
test.eq(s2.c, 'abc')
test.eq(s2.d, 'xyz')

type O(a, b int)

var o = O{1, 2}
test.eq(o.a, 1)
test.eq(o.b, 2)

type O2(a, b int, c, d string)

var o2 = O2{1, 2, 'abc', 'xyz'}
test.eq(o2.a, 1)
test.eq(o2.b, 2)
test.eq(o2.c, 'abc')
test.eq(o2.d, 'xyz')

--cytest: pass