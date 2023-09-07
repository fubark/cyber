-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

--|
--| Parentheses.
--|

-- Parentheses at left of binary expression.
t.eq((2 + 3) * 4, 20)

-- Parentheses at right of binary expression.
t.eq(2 * (3 + 4), 14)

-- Nested parentheses.
t.eq(2 + ((3 + 4) / 7), 3)

--|
--| Precedence=9, Bitwise shift
--|

-- Bitwise shift before bitwise and.
t.eq(int(2) & 1 << 1, int(2))

-- Bitwise shift before bitwise or.
t.eq(int(1) | 1 << 1, int(3))

--|
--| Precedence=8, Bitwise and
--|

-- Bitwise and before bitwise or.
t.eq(int(8) | 1 & 1, int(9))

--|
--| Precedence=7, Bitwise or, xor
--|

-- Bitwise or before power.
t.eq(int(3) ^ 2 | 1, int(27))

--|
--| Precedence=6, Power
--|

-- Power before multiplication.
t.eq(2 * 3 ^ 2, 18)

-- Power before addition.
t.eq(2 + 3 ^ 2, 11)

--|
--| Precedence=5, Divide, Modulus, Multiply
--|

-- Multiply before subtract, then subtract before greater comparison.
t.eq(5 > 6*1 - 3, true)

-- Multiplication before addition.
t.eq(2 + 3 * 4, 14)

-- Division before addition.
t.eq(2 + 4 / 4, 3)

-- Modulus before comparison.
t.eq(5 == 21 % 16, true)

--|
--| Precedence=4, Add, Subtract
--|

-- Add before comparison.
t.eq(5 == 2 + 3, true)

--|
--| Precedence=1, Logical and.
--|

-- Logical and before or.
t.eq(true or true and false, true)

--|
--| Other tests.
--|

-- Variables and parenthesis.
var time = 50
var minTime = 50
var timeRange = 100
t.eq(5 + 90 * (time - minTime) / timeRange, 5)

-- Left recursion with different operators.
t.eq(5 + 2 * 3 / 3, 7)