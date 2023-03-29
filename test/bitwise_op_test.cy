-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Bitwise and
t.eq(4 & 2, 0)
t.eq(4 & 4, 4)
t.eq(7 & 2, 2)

-- Bitwise or
t.eq(4 | 2, 6)
t.eq(4 | 4, 4)

-- Bitwise xor
t.eq(4 || 2, 6)
t.eq(4 || 4, 0)

-- Bitwise not
t.eq(~0, -1)
t.eq(~-1, 0)
t.eq(~1, -2)

-- Bitwise right shift
t.eq(16 >> 2, 4)
t.eq(2 >> 2, 0)
t.eq(-1 >> 1, -1)  -- Performs sign extension

-- Bitwise left shift
t.eq(2 << 4, 32)
t.eq(-2147483648 << 1, 0)  -- Does not perform sign extension