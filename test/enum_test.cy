-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- TagType to number.
type Animal enum:
  Bear
  Tiger
n = Animal#Tiger
try t.eq(number(n), 1)

-- Using TagType declared afterwards.
n = Animal#Tiger
type Animal enum:
  Bear
  Tiger
try t.eq(number(n), 1)

-- Reassign using tag literal.
type Animal enum:
  Bear
  Tiger
  Dragon
n = Animal#Tiger
n = #Dragon
try t.eq(number(n), 2)