-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'
import m 'math'

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
t.eq(-2147483648 >> 100, -134217728)
t.eq(-2147483648 >> 1000, -8388608)
t.eq(-2147483648 >> -1, -1)
t.eq(-2147483648 >> -4, -8)
t.eq(-2147483648 >> m.nan, -2147483648)
t.eq(-2147483648 >> m.inf, -2147483648)
t.eq(-1 >> 1, -1)  -- Performs sign extension

-- Bitwise left shift
t.eq(2 << 4, 32)
t.eq(1 << 100, 16)
t.eq(1 << 1000, 256)
t.eq(1 << -1, -2147483648)
t.eq(1 << -2, 1073741824)
t.eq(1 << m.nan, 1)
t.eq(1 << m.inf, 1)
t.eq(-2147483648 << 1, 0)  -- Does not perform sign extension