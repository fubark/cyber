use a 'test_mods/a.cy'
use test

-- Global.
use g_int = a.g_int
test.eq(g_int, 123)

-- Function.
use func = a.func
test.eq(func(), 234)

-- Type.
type Vec2 = a.Vec2
v := Vec2{x=1, y=2}
test.eq(v.x, 1.0)
test.eq(v.y, 2.0)

-- Type template.
use TFoo = a.TFoo
o := TFoo[int]{a=555}
test.eq(o.a, 555)

--cytest: pass