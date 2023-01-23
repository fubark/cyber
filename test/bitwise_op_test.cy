-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Bitwise and
try t.eq(4 & 2, 0)
try t.eq(4 & 4, 4)
try t.eq(7 & 2, 2)

-- Bitwise or
try t.eq(4 | 2, 6)
try t.eq(4 | 4, 4)

-- Bitwise xor
try t.eq(4 || 2, 6)
try t.eq(4 || 4, 0)

-- Bitwise not
try t.eq(~0, -1)
try t.eq(~-1, 0)
try t.eq(~1, -2)

-- Bitwise right shift
try t.eq(16 >> 2, 4)
try t.eq(2 >> 2, 0)
try t.eq(-1 >> 1, -1)  -- Performs sign extension

-- Bitwise left shift
try t.eq(2 << 4, 32)
try t.eq(-2147483648 << 1, 0)  -- Does not perform sign extension