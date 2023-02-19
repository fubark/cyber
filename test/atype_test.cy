-- Copyright (c) 2023 Cyber (See LICENSE)

import a './test_mods/a.cy'
import t 'test'

-- Type alias of imported type.
atype Vec2 a.Vec2

v = Vec2{ x: 1, y: 2 }
try t.eq(v.x, 1)
try t.eq(v.y, 2)