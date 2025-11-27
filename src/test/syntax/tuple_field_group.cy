use test

type S(a, b int)
s := S{1, 2}
test.eq(s.a, 1)
test.eq(s.b, 2)

type S2(a, b int, c, d str)
s2 := S2{1, 2, 'abc', 'xyz'}
test.eq(s2.a, 1)
test.eq(s2.b, 2)
test.eq(s2.c, 'abc')
test.eq(s2.d, 'xyz')

--cytest: pass