-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

type Animal enum:
  Bear
  Tiger

-- enum to float.
var n = Animal.Tiger
t.eq(float(n), 1)

-- Using enum declared afterwards.
n = Animal2.Tiger
t.eq(float(n), 1)

-- Reassign using symbol literal.
n = Animal2.Tiger
n = #Dragon
t.eq(float(n), 2)

type Animal2 enum:
  Bear
  Tiger
  Dragon