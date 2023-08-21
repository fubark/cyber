-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Basic.
var list = [1, 2, 3]
var sum = 0
for list each it:
   sum += it
t.eq(sum, 6)

-- From static iterable.
var sList: [1, 2, 3]
sum = 0
for sList each it:
   sum += it
t.eq(sum, 6)

-- Loop iterator var overwrites the user var.
var elem = 123
list = [1, 2, 3]
for list each elem:
  pass
t.eq(elem, none)

-- Break.
list = [1, 2, 3]
sum = 0
for list each it:
   if it == 3:
      break
   sum += it
t.eq(sum, 3)

-- Continue.
list = [1, 2, 3]
sum = 0
for list each it:
   if it == 1:
      continue
   sum += it
t.eq(sum, 5)

-- Single line block.
list = [1, 2, 3]
sum = 0
for list each it: sum += it
t.eq(sum, 6)

-- Return expr inside loop body.
list = [1, 2, 3]
var f = func (arr):
   for arr each item:
      if item == 4:
         return 1
      else item == 5:
         return 1
   return 0
t.eq(f(list), 0)

-- Empty iterator. Tests that iterator is cleaned up without entering body loop.
list = []
var count = 0
for list each it:
   count += 1
t.eq(count, 0)