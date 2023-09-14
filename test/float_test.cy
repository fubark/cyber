-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'
import m 'math'

-- Default notation.
t.eq(1.0, 1.0)
t.eq(-1.0, -1.0)
t.eq(-(-1.0), 1.0)

-- Scientific notation.
t.eq(1.23e2, 123.0)
t.eqNear(123e-2, 1.23)

-- float.$call()
t.eq(float(100), 100.0)
t.eq(float(100.1), 100.1)
t.eq(float('100'), 100.0)
t.eq(float('100.1'), 100.1)
t.eq(float(none), 0.0)
t.eq(float(true), 1.0)
t.eq(float(false), 0.0)

-- Infinity.
t.eq(1.0 / 0, m.inf)

-- NaN.
t.eq(m.isNaN(0.0 * (1.0 / 0)), true)
