-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

--|
--| Precedence=5, Power, Divide, Modulus, Multiply
--|

-- Multiply before subtract, then subtract before greater comparison.
try t.eq(5 > 6*1 - 3, true)

-- Multiplication before addition.
try t.eq(2 + 3 * 4, 14)

-- Division before addition.
try t.eq(2 + 4 / 4, 3)

-- Power before addition.
try t.eq(2 + 3 ^ 2, 11)

-- Modulus before comparison.
try t.eq(5 == 21 % 16, true)

-- Power before multiplication.
try t.eq(2 * 3 ^ 2, 18)

--|
--| Precedence=4, Add, Subtract
--|

-- Add before comparison.
try t.eq(5 == 2 + 3, true)

--|
--| Precendence=3, Bitwise shift
--|

-- Bitwise shift before bitwise or.
try t.eq(1 | 1 << 1, 3)

-- Variables and parenthesis.
time = 50
minTime = 50
timeRange = 100
try t.eq(5 + 90 * (time - minTime) / timeRange, 5)

-- Left recursion with different operators.
try t.eq(5 + 2 * 3 / 3, 7)