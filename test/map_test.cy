-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Empty map.
var a = [:]
t.eq(a.size(), 0)
t.eq(a['foo'], none)

-- Omits last comma for multiline initializer.
a = [
    foo: 123,
    bar: 234,
]
-- Get size.
t.eq(a.size(), 2)

-- String literal in initializer.
a = [
    'c': 234
]
t.eq(a['c'], 234)
t.eq(a.c, 234)

--| Different string keys.
a = [:]
-- astring.
a['abc'] = 123
t.eq(a['abc'], 123)
-- ustring.
a['abc🦊'] = 123
t.eq(a['abc🦊'], 123)
-- astring expr.
a['abc' + 'd'] = 123
t.eq(a['abcd'], 123)
-- ustring expr.
a['abc🦊' + 'd'] = 123
t.eq(a['abc🦊d'], 123)
-- astring slice
a['abc'[1..]] = 123
t.eq(a['bc'], 123)
-- ustring slice
a['abc🦊'[1..]] = 123
t.eq(a['bc🦊'], 123)
-- array
a[array('abc')] = 123
t.eq(a['abc'], 123)
-- array slice
a[array('abc')[1..]] = 123
t.eq(a['bc'], 123)

-- Number key.
a[32] = 123
t.eq(a[32], 123)

-- Access expression.
a = [
    b: 32
]
t.eq(a.b, 32)

-- String entry.
a = [
    b: 'hello'
]
t.eq(a['b'], 'hello')

-- Add to empty map.
a = [:]
a[123] = 234
t.eq(a[123], 234)

-- Nested list.
a = [
    b: [ 1, 2 ]
]
t.eq(a.b[1], 2)

-- Nested list with items separated by new line.
a = [
    b: [
        1,
        2,
    ]
]
t.eq(a.b[1], 2)

-- Iterate maps.

-- Iterator.
var m = [ a: 2, b: 3, c: 4 ]
var sum = 0
for m -> entry:
    sum += entry[1]
t.eq(sum, 9)

-- Destructure map entry.
sum = 0
var codeSum = 0
for m -> [k, v]:
    sum += v
    codeSum += k.runeAt(0)
t.eq(sum, 9)
t.eq(codeSum, 294)

-- Nested iteration.
m = [ a: 1, b: 2, c: 3 ]
var res = 0
for m -> e1:
    var innerSum = 0
    for m -> e2:
        innerSum += e2[1] 
    res += e1[1] * innerSum
t.eq(res, 36)

-- Nested iteration with destructuring.
m = [ a: 1, b: 2, c: 3 ]
res = 0
codeSum = 0
for m -> [k, n]:
    var innerSum = 0
    for m -> [kk, nn]:
        innerSum += nn
        codeSum += kk.runeAt(0)
    res += n * innerSum
    codeSum += k.runeAt(0)
t.eq(res, 36)
t.eq(codeSum, 294 * 4)

-- Iterate rc values.
m = [ a: [2], b: [3], c: [4] ]
sum = 0
codeSum = 0
for m -> [k, v]:
    sum += v[0]
    codeSum += k.runeAt(0)
t.eq(sum, 9)
t.eq(codeSum, 294)

-- Remove from map.
m = [ a: 2, b: 3, c: 4 ]
m.remove('a')
t.eq(m.size(), 2)
t.eq(m['a'], none)
t.eq(m['b'], 3)
t.eq(m['c'], 4)

-- Remove rc key from map.
m = [:]
m[string(1)] = 123
m.remove('1')
t.eq(m.size(), 0)