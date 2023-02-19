-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Omits last comma for multiline initializer.
a = {
  foo: 123,
  bar: 234,
}
try t.eq(a.size(), 2)

a = {
    b: 123
    'c': 234
}
-- Get size.
try t.eq(a.size(), 2)

-- Number entry.
a = {
  b: 32
}
try t.eq(a['b'], 32)

-- Access expression.
a = {
  b: 32
}
try t.eq(a.b, 32)

-- String entry.
a = {
  b: 'hello'
}
try t.eq(a['b'], 'hello')

-- Add to empty map.
a = {}
a[123] = 234
try t.eq(a[123], 234)

-- Nested list.
a = {
  b: [ 1, 2 ]
}
try t.eq(a.b[1], 2)

-- Nested list with items separated by new line.
a = {
  b: [
    1
    2
  ]
}
try t.eq(a.b[1], 2)

-- Iterate maps.

-- Iterator.
m = { a: 2, b: 3, c: 4 }
sum = 0
for m each v:
  sum += v 
try t.eq(sum, 9)

-- Pair Iterator.
sum = 0
codeSum = 0
for m each k, v:
  sum += v
  codeSum += asciiCode(k)
try t.eq(sum, 9)
try t.eq(codeSum, 294)

-- Nested iteration.
m = { a: 1, b: 2, c: 3 }
res = 0
for m each n:
  innerSum = 0
  for m each nn:
    innerSum += nn
  res += n * innerSum
try t.eq(res, 36)

-- Nested pair iteration.
m = { a: 1, b: 2, c: 3 }
res = 0
codeSum = 0
for m each k, n:
  innerSum = 0
  for m each kk, nn:
    innerSum += nn
    codeSum += asciiCode(kk)
  res += n * innerSum
  codeSum += asciiCode(k)
try t.eq(res, 36)
try t.eq(codeSum, 294 * 4)

-- Iterate rc values.
m = { a: [2], b: [3], c: [4] }
sum = 0
codeSum = 0
for m each k, v:
  sum += v[0]
  codeSum += asciiCode(k)
try t.eq(sum, 9)
try t.eq(codeSum, 294)

-- Remove from map.
m = { a: 2, b: 3, c: 4 }
m.remove('a')
try t.eq(m.size(), 2)
try t.eq(m['a'], none)
try t.eq(m['b'], 3)
try t.eq(m['c'], 4)