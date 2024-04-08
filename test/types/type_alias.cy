use t 'test'

type Vec2:
    x float
    y float

type Vec = Vec2

var v = Vec{x: 1, y: 2}
t.eq(v.x, 1.0)
t.eq(v.y, 2.0)

func foo(v Vec2):
    pass

func foo2(v Vec):
    pass

foo(v)
foo2(v)

--cytest: pass