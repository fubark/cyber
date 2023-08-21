-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Basic.
var iters = 0
for 0..10 each i:
   iters += 1
t.eq(iters, 10)

-- two `for` with range don't interfere with each other
iters = 0
for 0..10 each i:
   iters += 1
for 0..10 each i:
   iters += 1
t.eq(iters, 20)

-- two `for` with non const max value don't interfere with each other
var foo = 10
iters = 0
for 0..foo each i:
   iters += 1
for 0..foo each i:
   iters += 1
t.eq(iters, 20)

-- Nested for loop.
var count = 0
for 0..10 each i:
  var inner = 0
  for 0..10 each j:
    inner += 1
  count += inner
t.eq(count, 100)

-- Index vars overwrites user var.
var i = 123
var sum = 0
for 0..10 each i:
  sum += i
t.eq(i, 9)

-- Reverse direction.
sum = 0
for 10..0 each i:
  sum += i
t.eq(sum, 55)

-- Break.
iters = 0
for 0..10 each i:
   if i == 2:
       break
   iters += 1
t.eq(iters, 2)

-- Continue.
iters = 0
for 0..10 each i:
   if i == 2:
       continue
   iters += 1
t.eq(iters, 9)

-- Single line block.
iters = 0
for 0..10 each i: iters += 1
t.eq(iters, 10)