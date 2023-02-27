-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Addition.
try t.eq(1 + 2, 3)
try t.eq(1 + 2 + 3, 6)

-- Subtract.
try t.eq(3 - 1, 2)

-- Multiply.
try t.eq(3 * 4, 12)

-- Divide.
try t.eq(20 / 5, 4)

-- Power.
try t.eq(2 ^ 5, 32)

-- Modulus.
try t.eq(3 % 2, 1)

-- Assign to same var.
a = 0
a = a + 1
try t.eq(a, 1)
a = 0
a = 1 + a
try t.eq(a, 1)

-- Right function call.
func foo():
  return 123
try t.eq(1 + foo(), 124)