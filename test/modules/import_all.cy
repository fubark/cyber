use * 'test_mods/a.cy'
use test

-- Namespace var.
test.eq(varInt, 123)

-- Namespace func.
test.eq(fn(), 234)

-- Type.
var v = Vec2{x: 1, y: 2}
test.eq(v.x, 1.0)
test.eq(v.y, 2.0)

-- Type template.
var o = TFoo(int){a: 555}
test.eq(o.a, 555)

--cytest: pass