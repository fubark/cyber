use * 'test_mods/a.cy'
use test

-- Namespace var.
test.eq(g_int, 123)

-- Namespace func.
test.eq(func(), 234)

-- Type.
v := Vec2{x=1, y=2}
test.eq(v.x, 1.0)
test.eq(v.y, 2.0)

-- Type template.
o := TFoo[int]{a=555}
test.eq(o.a, 555)

--cytest: pass