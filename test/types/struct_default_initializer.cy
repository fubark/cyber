use test

type Foo:
    a int = 123
    b int

foo := Foo{b=321}
test.eq(123, foo.a)
test.eq(321, foo.b)

-- Array, Map
type Foo2:
    a int   = 0
    b Array[int] = {}
    c Map[str, int] = {}
foo2 := Foo2{}
test.eq(0, foo2.a)
test.eq(0, foo2.b.len())
test.eq(0, foo2.c.size())

--cytest: pass