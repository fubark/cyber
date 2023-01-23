-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

try t.eq(1 < 2, true)
try t.eq(2 < 1, false)
try t.eq(1 > 2, false)
try t.eq(2 > 1, true)
try t.eq(1 <= 2, true)
try t.eq(2 <= 2, true)
try t.eq(3 <= 2, false)
try t.eq(1 >= 2, false)
try t.eq(2 >= 2, true)
try t.eq(3 >= 2, true)