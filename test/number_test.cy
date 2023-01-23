-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

try t.eq(1, 1)
try t.eq(-1, -1)
try t.eq(-(-1), 1)
try t.eq(1.23e2, 123)
try t.eqNear(123e-2, 1.23)
try t.eq(0xff, 255)
try t.eq(0xFF, 255)
try t.eq(0o77, 63)
try t.eq(0b11, 3)