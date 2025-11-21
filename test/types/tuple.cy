use test

type S struct(a int, b str)

s := S{123, 'abc'}
test.eq(s.a, 123)
test.eq(s.b, 'abc')

s = {234, 'xyz'}
test.eq(s.a, 234)
test.eq(s.b, 'xyz')

s = S{a=123, b='abc'}
test.eq(s.a, 123)
test.eq(s.b, 'abc')

s = {a=234, b='xyz'}
test.eq(s.a, 234)
test.eq(s.b, 'xyz')

-- Struct tuple with functions.
type S2 struct(a int, b str)

fn (&S2) foo():
    pass

type O(a int, b str)

o := O{123, 'abc'}
test.eq(o.a, 123)
test.eq(o.b, 'abc')

o = {234, 'xyz'}
test.eq(o.a, 234)
test.eq(o.b, 'xyz')

o = O{a=123, b='abc'}
test.eq(o.a, 123)
test.eq(o.b, 'abc')

o = {a=234, b='xyz'}
test.eq(o.a, 234)
test.eq(o.b, 'xyz')

-- Shorthand.
type S3(a int, b str)

fn (&S3) foo():
    pass

--cytest: pass