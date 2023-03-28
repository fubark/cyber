-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

type Animal enum:
  Bear
  Tiger

-- enum to number.
n = Animal.Tiger
try t.eq(number(n), 1)

-- Using enum declared afterwards.
n = Animal2.Tiger
try t.eq(number(n), 1)

-- Reassign using symbol literal.
n = Animal2.Tiger
n = #Dragon
try t.eq(number(n), 2)

type Animal2 enum:
  Bear
  Tiger
  Dragon