-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'
import m 'math'

-- Bitwise and
t.eq(int(4) & 2, int(0))
t.eq(int(4) & 4, int(4))
t.eq(int(7) & 2, int(2))

-- Bitwise or
t.eq(int(4) | 2, int(6))
t.eq(int(4) | 4, int(4))

-- Bitwise xor
t.eq(int(4) || 2, int(6))
t.eq(int(4) || 4, int(0))

-- Bitwise not
t.eq(~int(0), int(-1))
t.eq(~int(-1), int(0))
t.eq(~int(1), int(-2))

-- Bitwise right shift
t.eq(int(16) >> 2, int(4))
t.eq(int(2) >> 2, int(0))
t.eq(int(-2147483648) >> 100, int(-134217728))
t.eq(int(-2147483648) >> 1000, int(-8388608))
t.eq(int(-2147483648) >> -1, int(-1))
t.eq(int(-2147483648) >> -4, int(-8))
t.eq(int(-1) >> 1, int(-1))  -- Performs sign extension

-- Bitwise left shift
t.eq(int(2) << 4, int(32))
t.eq(int(1) << 100, int(16))
t.eq(int(1) << 1000, int(256))
t.eq(int(1) << -1, int(-2147483648))
t.eq(int(1) << -2, int(1073741824))
t.eq(int(-2147483648) << 1, int(0))  -- Does not perform sign extension