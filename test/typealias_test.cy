-- Copyright (c) 2023 Cyber (See LICENSE)

import a './test_mods/a.cy'
import t 'test'

-- Type alias of imported type.
type Vec2 a.Vec2

-- Using alias as type spec.
type Parent object:
    v Vec2

var v = Vec2{ x: 1, y: 2 }
t.eq(v.x, 1)
t.eq(v.y, 2)

func foo(v Vec2):
    pass

-- Using alias from imported module.
v = a.Vec2Alias{ x: 1, y: 2 }

-- Using alias from imported module as type spec.
type Parent2 object:
    v a.Vec2Alias