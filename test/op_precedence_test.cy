-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

--|
--| Parentheses.
--|

-- Parentheses at left of binary expression.
try t.eq((2 + 3) * 4, 20)

-- Parentheses at right of binary expression.
try t.eq(2 * (3 + 4), 14)

-- Nested parentheses.
try t.eq(2 + ((3 + 4) / 7), 3)

--|
--| Precedence=9, Bitwise shift
--|

-- Bitwise shift before bitwise and.
try t.eq(2 & 1 << 1, 2)

-- Bitwise shift before bitwise or.
try t.eq(1 | 1 << 1, 3)

--|
--| Precedence=8, Bitwise and
--|

-- Bitwise and before bitwise or.
try t.eq(8 | 1 & 1, 9)

--|
--| Precedence=7, Bitwise or, xor
--|

-- Bitwise or before power.
try t.eq(3 ^ 2 | 1, 27)

--|
--| Precedence=6, Power
--|

-- Power before multiplication.
try t.eq(2 * 3 ^ 2, 18)

-- Power before addition.
try t.eq(2 + 3 ^ 2, 11)

--|
--| Precedence=5, Divide, Modulus, Multiply
--|

-- Multiply before subtract, then subtract before greater comparison.
try t.eq(5 > 6*1 - 3, true)

-- Multiplication before addition.
try t.eq(2 + 3 * 4, 14)

-- Division before addition.
try t.eq(2 + 4 / 4, 3)

-- Modulus before comparison.
try t.eq(5 == 21 % 16, true)

--|
--| Precedence=4, Add, Subtract
--|

-- Add before comparison.
try t.eq(5 == 2 + 3, true)

--|
--| Precedence=2, Logical and.
--|

-- Logical and before or.
try t.eq(true or true and false, true)

--|
--| Other tests.
--|

-- Variables and parenthesis.
time = 50
minTime = 50
timeRange = 100
try t.eq(5 + 90 * (time - minTime) / timeRange, 5)

-- Left recursion with different operators.
try t.eq(5 + 2 * 3 / 3, 7)