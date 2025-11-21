use test

type B:
    a int
    b float

type A:
    a int
    b B
    c float

a := A{
    a = 123,
    b = {a=234, b=100.0},
    c = 200.0,
}

test.eq(a.a, 123)
test.eq(a.b.a, 234)
test.eq(a.b.b, 100.0)
test.eq(a.c, 200.0)

--cytest: pass