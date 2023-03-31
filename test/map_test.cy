-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Omits last comma for multiline initializer.
a = {
  foo: 123,
  bar: 234,
}
-- Get size.
t.eq(a.size(), 2)

-- String literal in initializer.
a = {
  'c': 234
}
t.eq(a['c'], 234)
t.eq(a.c, 234)

--| Different string keys.
a = {}
-- Static astring
a['abc'] = 123
t.eq(a['abc'], 123)
-- Static ustring
a['abc🦊'] = 123
t.eq(a['abc🦊'], 123)
-- astring
a['abc'.concat('d')] = 123
t.eq(a['abcd'], 123)
-- ustring
a['abc🦊'.concat('d')] = 123
t.eq(a['abc🦊d'], 123)
-- astring slice
a['abc'[1..]] = 123
t.eq(a['bc'], 123)
-- ustring slice
a['abc🦊'[1..]] = 123
t.eq(a['bc🦊'], 123)
-- rawstring
a[rawstring('abc')] = 123
t.eq(a['abc'], 123)
-- rawstring slice
a[rawstring('abc')[1..]] = 123
t.eq(a['bc'], 123)

-- Number key.
a[32] = 123
t.eq(a[32], 123)

-- Access expression.
a = {
  b: 32
}
t.eq(a.b, 32)

-- String entry.
a = {
  b: 'hello'
}
t.eq(a['b'], 'hello')

-- Add to empty map.
a = {}
a[123] = 234
t.eq(a[123], 234)

-- Nested list.
a = {
  b: [ 1, 2 ]
}
t.eq(a.b[1], 2)

-- Nested list with items separated by new line.
a = {
  b: [
    1
    2
  ]
}
t.eq(a.b[1], 2)

-- Iterate maps.

-- Iterator.
m = { a: 2, b: 3, c: 4 }
sum = 0
for m each v:
  sum += v 
t.eq(sum, 9)

-- Pair Iterator.
sum = 0
codeSum = 0
for m each k, v:
  sum += v
  codeSum += k.runeAt(0)
t.eq(sum, 9)
t.eq(codeSum, 294)

-- Nested iteration.
m = { a: 1, b: 2, c: 3 }
res = 0
for m each n:
  innerSum = 0
  for m each nn:
    innerSum += nn
  res += n * innerSum
t.eq(res, 36)

-- Nested pair iteration.
m = { a: 1, b: 2, c: 3 }
res = 0
codeSum = 0
for m each k, n:
  innerSum = 0
  for m each kk, nn:
    innerSum += nn
    codeSum += kk.runeAt(0)
  res += n * innerSum
  codeSum += k.runeAt(0)
t.eq(res, 36)
t.eq(codeSum, 294 * 4)

-- Iterate rc values.
m = { a: [2], b: [3], c: [4] }
sum = 0
codeSum = 0
for m each k, v:
  sum += v[0]
  codeSum += k.runeAt(0)
t.eq(sum, 9)
t.eq(codeSum, 294)

-- Remove from map.
m = { a: 2, b: 3, c: 4 }
m.remove('a')
t.eq(m.size(), 2)
t.eq(m['a'], none)
t.eq(m['b'], 3)
t.eq(m['c'], 4)