-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Assign to variable.
a = 0 
next = func():
  if a < 4:
    a += 1
    return a
  else:
    return none
sum = 0
for next() as res:
  sum += res
try t.eq(sum, 10)

-- Assign rc value to variable.
a = 0 
next = func ():
  if a < 4:
    a += 1
    return [a]
  else:
    return none
sum = 0
for next() as res:
  sum += res[0]
try t.eq(sum, 10)

-- Single line block.
a = 0 
next = func():
  if a < 4:
    a += 1
    return a
  else:
    return none
sum = 0
for next() as res: sum += res
try t.eq(sum, 10)