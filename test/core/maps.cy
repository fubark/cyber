use t 'test'

-- Empty map.
var a = Map{}
t.eq(a.size(), 0)

-- Omits last comma for multiline initializer.
a = Map{
    foo = 123,
    bar = 234,
}
-- Get size.
t.eq(a.size(), 2)

-- String literal in initializer.
a = Map{
    'c' = 234
}
t.eq(a['c'], 234)

--| Different string keys.
a = Map{}
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
-- Array
a[Array('abc')] = 123
t.eq(a['abc'], 123)
-- Array slice
a[Array('abc')[1..]] = 123
t.eq(a['bc'], 123)

-- Various key types.
a[32] = 1
t.eq(a[32], 1)
a[1.02] = 2
t.eq(a[1.02], 2)
a[0xff] = 3
t.eq(a[0xff], 3)

-- Indexing ident key.
a = Map{
    b = 32
}
t.eq(a['b'], 32)

-- Nested indexing.
a = Map{a = Map{b=5}}
t.eq(a['a']['b'], 5)

-- String entry.
a = Map{
    b = 'hello'
}
t.eq(a['b'], 'hello')

-- Add to empty map.
a = Map{}
a[123] = 234
t.eq(a[123], 234)

-- Nested list.
a = Map{
    b = {1, 2}
}
t.eq(a['b'][1], 2)

-- Nested list with items separated by commas.
a = Map{
    b = {
        1,
        2,
    }
}
t.eq(a['b'][1], 2)

-- Iterate maps.

-- Iterator.
var m = Map{a=2, b=3, c=4}
var sum = 0
for m -> entry:
    sum += entry[1]
t.eq(sum, 9)

-- Destructure map entry.
sum = 0
var codeSum = 0
for m -> {k, v}:
    sum += v
    codeSum += k[0]
t.eq(sum, 9)
t.eq(codeSum, 294)

-- Nested iteration.
m = Map{a=1, b=2, c=3}
var res = 0
for m -> e1:
    var innerSum = 0
    for m -> e2:
        innerSum += e2[1] 
    res += e1[1] * innerSum
t.eq(res, 36)

-- Nested iteration with destructuring.
m = Map{a=1, b=2, c=3}
res = 0
codeSum = 0
for m -> {k, n}:
    var innerSum = 0
    for m -> {kk, nn}:
        innerSum += nn
        codeSum += kk[0]
    res += n * innerSum
    codeSum += k[0]
t.eq(res, 36)
t.eq(codeSum, 294 * 4)

-- Iterate rc values.
m = Map{a={2}, b={3}, c={4}}
sum = 0
codeSum = 0
for m -> {k, v}:
    sum += v[0]
    codeSum += k[0]
t.eq(sum, 9)
t.eq(codeSum, 294)

-- Remove from map.
m = Map{a=2, b=3, c=4}
t.eq(m.get('a').?, 2)
m.remove('a')
t.eq(m.size(), 2)
t.assert(m.get('a') == none)
t.eq(m['b'], 3)
t.eq(m['c'], 4)

-- Remove rc key from map.
m = Map{}
m[String(1)] = 123
m.remove('1')
t.eq(m.size(), 0)

-- contains()
m = Map{a=123}
t.eq(m.contains('a'), true)
t.eq(m.contains('b'), false)

-- get()
m = Map{a=123}
t.eq(m.get('a').?, 123)
t.assert(m.get('b') == none)

--cytest: pass