-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'
import m 'math'

-- Addition.
t.eq(1 + 2, 3)
t.eq(1 + 2 + 3, 6)

-- Subtract.
t.eq(3 - 1, 2)

-- Multiply.
t.eq(3 * 4, 12)

-- Divide.
t.eq(20 / 5, 4)

-- Power.
t.eq(2 ^ 5, 32)

-- Modulus.
t.eq(3 % 2, 1)

-- Assign to same var.
var a = 0
a = a + 1
t.eq(a, 1)
a = 0
a = 1 + a
t.eq(a, 1)

-- Right function call.
func foo():
  return 123
t.eq(1 + foo(), 124)