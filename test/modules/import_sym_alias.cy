use a 'test_mods/a.cy'
use test

-- Static var.
use varInt -> a.varInt
test.eq(varInt, 123)

-- Static func.
use func -> a.func
test.eq(func(), 234)

-- Type.
use Vec2 -> a.Vec2
var v = Vec2{x=1, y=2}
test.eq(v.x, 1.0)
test.eq(v.y, 2.0)

-- Type template.
use TFoo -> a.TFoo
var o = TFoo[int]{a=555}
test.eq(o.a, 555)

--cytest: pass