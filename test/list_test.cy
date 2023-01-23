-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Index access.
a = [1, 2, 3]
try t.eq(a[0], 1)

-- Reverse index access.
try t.eq(a[-1], 3)

-- Slice operator.
-- Start to end index slice.
a = [1, 2, 3, 4, 5]
try t.eqList(a[1..4], [2, 3, 4])
-- Start index to end of list.
try t.eqList(a[3..], [4, 5])
-- Start of list to end index.
try t.eqList(a[..3], [1, 2, 3])

-- Set index
a = []
a.resize(3)
a[2] = 3
try t.eq(a[2], 3)

-- append()
a = []
a.append(1)
try t.eq(a.len(), 1)
try t.eq(a[0], 1)

-- concat()
a = [1, 2, 3]
a.concat([4, 5, 6])
try t.eqList(a, [1, 2, 3, 4, 5, 6])

-- insert() in empty
a = []
a.insert(0, 1)
try t.eq(a[0], 1)

-- insert() at start
a.insert(0, 2)
try t.eqList(a, [2, 1])

-- insert() at end
a.insert(2, 3)
try t.eqList(a, [2, 1, 3])

-- insert() in middle
a.insert(1, 4)
try t.eqList(a, [2, 4, 1, 3])

-- insert() at index out of bounds.
try t.eq(a.insert(-1, 123), error(#OutOfBounds))
try t.eq(a.insert(100, 123), error(#OutOfBounds))

-- joinString()
try t.eq([].joinString(','), '')
try t.eq([1].joinString(','), '1')
try t.eq([1, 2, 3].joinString(','), '1,2,3')
try t.eq([1, 2, 3].joinString(',').isAscii(), true)
try t.eq([1, 2, 3].joinString('🦊'), '1🦊2🦊3')
try t.eq([1, 2, 3].joinString('🦊').isAscii(), false)

-- len()
a = [1, 2, 3, 4]
try t.eq(a.len(), 4)

-- remove()
a = [1, 2, 3]
a.remove(1)
try t.eq(a.len(), 2)
try t.eq(a[0], 1)
try t.eq(a[1], 3)

-- remove() first item.
a = [1, 2, 3]
a.remove(0)
try t.eq(a.len(), 2)
try t.eq(a[0], 2)
try t.eq(a[1], 3)

-- remove() last item.
a = [1, 2, 3]
a.remove(2)
try t.eq(a.len(), 2)
try t.eq(a[0], 1)
try t.eq(a[1], 2)

-- remove() out of bounds.
a = [1, 2, 3]
try t.eq(a.remove(-1), error(#OutOfBounds))
try t.eq(a.remove(3), error(#OutOfBounds))
try t.eq(a.len(), 3)

-- remove() rc item.
a = [1, [123], 3]
a.remove(1)
try t.eq(a.len(), 2)
try t.eq(a[0], 1)
try t.eq(a[1], 3)

-- resize()
a = [1, 2, 3]
a.resize(4)
try t.eq(a.len(), 4)
try t.eq(a[3], none)
a.resize(2)
try t.eq(a.len(), 2)
try t.eq(a[1], 2)

-- sort()
a = [3, 1, 2]
a.sort((a, b) => a < b)
try t.eqList(a, [1, 2, 3])
a = [[3], [1], [2]]
a.sort((a, b) => a[0] < b[0])
try t.eq(a[0][0], 1)
try t.eq(a[1][0], 2)
try t.eq(a[2][0], 3)

-- Iteration.
let a = [1, 2, 3, 4, 5]
let sum = 0
for a each it:
  sum += it
try t.eq(sum, 15)

-- Pair iteration.
a = [10, 20, 30]
sum = 0
let idxSum = 0
for a each idx, it:
  sum += it
  idxSum += idx
try t.eq(sum, 60)
try t.eq(idxSum, 3)

-- Nested iteration.
a = [1, 2, 3]
res = 0
for a each n:
  innerSum = 0
  for a each m:
    innerSum += m
  res += n * innerSum
try t.eq(res, 36)

-- Nested pair iteration.
a = [1, 2, 3]
res = 0
idxRes = 0
for a each i, n:
  innerSum = 0
  idxSum = 0
  for a each j, m:
    innerSum += m
    idxSum += j
  res += n * innerSum
  idxRes += i * idxSum
try t.eq(res, 36)
try t.eq(idxRes, 9)