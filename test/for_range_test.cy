import t 'test'

-- Basic.
iters = 0
for 0..10 each i:
   iters += 1
try t.eq(iters, 10)

-- two `for` with range don't interfere with each other
iters = 0
for 0..10 each i:
   iters += 1
for 0..10 each i:
   iters += 1
try t.eq(iters, 20)

-- two `for` with non const max value don't interfere with each other
foo = 10
iters = 0
for 0..foo each i:
   iters += 1
for 0..foo each i:
   iters += 1
try t.eq(iters, 20)

-- Nested for loop.
count = 0
for 0..10 each i:
  inner = 0
  for 0..10 each j:
    inner += 1
  count += inner
try t.eq(count, 100)

-- Index vars overwrites user var.
i = 123
sum = 0
for 0..10 each i:
  sum += i
try t.eq(i, 9)

-- Reverse direction.
sum = 0
for 10..0 each i:
  sum += i
try t.eq(sum, 55)

-- Break.
iters = 0
for 0..10 each i:
   if i == 2:
       break
   iters += 1
try t.eq(iters, 2)

-- Continue.
iters = 0
for 0..10 each i:
   if i == 2:
       continue
   iters += 1
try t.eq(iters, 9)