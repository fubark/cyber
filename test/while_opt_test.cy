-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Assign to variable.
a = 0 
next = func():
  capture a
  if a < 4:
    a += 1
    return a
  else:
    return none
sum = 0
while next() some res:
  sum += res
t.eq(sum, 10)

-- Assign rc value to variable.
a = 0 
next = func ():
  capture a
  if a < 4:
    a += 1
    return [a]
  else:
    return none
sum = 0
while next() some res:
  sum += res[0]
t.eq(sum, 10)

-- Single line block.
a = 0 
next = func():
  capture a
  if a < 4:
    a += 1
    return a
  else:
    return none
sum = 0
while next() some res: sum += res
t.eq(sum, 10)