use t 'test'

-- Basic.
var iters = 0
for 0..10 -> i:
   iters += 1
t.eq(iters, 10)

-- two `for` with range don't interfere with each other
iters = 0
for 0..10 -> i:
   iters += 1
for 0..10 -> i:
   iters += 1
t.eq(iters, 20)

-- two `for` with non const max value don't interfere with each other
var foo = 10
iters = 0
for 0..foo -> i:
   iters += 1
for 0..foo -> i:
   iters += 1
t.eq(iters, 20)

-- Nested for loop.
var count = 0
for 0..10 -> i:
  var inner = 0
  for 0..10 -> j:
    inner += 1
  count += inner
t.eq(count, 100)

-- Index vars shadow parent vars.
var i = 123
var sum = 0
for 0..10 -> i:
  sum += i
t.eq(sum, 45)
t.eq(i, 123)

-- Reverse direction.
sum = 0
for 10-..0 -> i:
  sum += i
t.eq(sum, 55)

-- Break.
iters = 0
for 0..10 -> i:
    if i == 2:
         break
    iters += 1
t.eq(iters, 2)

-- Break releases current block vars.
for 0..10 -> i:
    if i == 2:
        dyn a = {_}
        break

-- Continue.
iters = 0
for 0..10 -> i:
   if i == 2:
       continue
   iters += 1
t.eq(iters, 9)

-- Continue releases current block vars.
for 0..10 -> i:
    if i == 2:
        dyn a = {_}
        continue

-- Single line block.
iters = 0
for 0..10 -> i: iters += 1
t.eq(iters, 10)

--cytest: pass