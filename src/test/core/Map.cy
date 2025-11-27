use t 'test'

-- Empty map.
a := Map[str, int]{}
t.eq(0, a.size())

-- Ignores last comma for multiline initializer.
a = {
    foo = 123,
    bar = 234,
}
-- Get size.
t.eq(2, a.size())

-- String literal in initializer.
a = {
    'c' = 234
}
t.eq(a['c'], 234)

-- Str keys.
a = {}
a['abc'] = 123
t.eq(123, a['abc'])

-- int key.
int_key_m := Map[int, int]{}
int_key_m[32] = 1
t.eq(1, int_key_m[32])
int_key_m[0xff] = 3
t.eq(3, int_key_m[0xff])

-- float key.
float_key_m := Map[float, int]{}
float_key_m[1.02] = 2
t.eq(2, float_key_m[1.02])

-- Indexing ident key.
a = {
    b = 32
}
t.eq(a['b'], 32)

-- Nested `Map`.
b := Map[str, ^Map[str,int]]{}
b['a'] = ^Map[str, int]{b=5}
t.eq(5, b['a']['b'])

-- Nested `Array`.
c := Map[str, ^Array[int]]{
    b = ^{1, 2}
}
t.eq(2, c['b'][1])

-- Nested `^ArrayStore`.
d := Map[str, ^ArrayStore[int]]{
    b = ^{1, 2}
}
t.eq(2, d['b'][1])
d['b'] = ^{}
t.eq(0, d['b'].len())

-- str value.
str_val_m := Map[str, str]{
    b = 'hello'
}
t.eq('hello', str_val_m['b'])

-- Iterator.
m := Map[str, int]{a=2, b=3, c=4}
sum := 0
codeSum := 0
for m |entry|:
    sum += entry.value
    codeSum += int(entry.key[0])
t.eq(9, sum)

-- Nested iteration.
m = {a=1, b=2, c=3}
res := 0
codeSum = 0
for m |e1|:
    innerSum := 0
    for m |e2|:
        innerSum += e2.value
        codeSum += int(e2.key[0])
    res += innerSum * e1.value
    codeSum += int(e1.key[0])
t.eq(36, res)
t.eq(294*4, codeSum)

-- Iterate rc values.
span_m := Map[str, ^int]{a=^2, b=^3, c=^4}
sum = 0
codeSum = 0
for span_m |e|:
    sum += e.value.*
    codeSum += int(e.key[0])
t.eq(9, sum)
t.eq(294, codeSum)

-- Remove from map.
m = {a=2, b=3, c=4}
t.eq(m.get('a').?, 2)
m.remove('a')
t.eq(m.size(), 2)
t.assert(m.get('a') == none)
t.eq(m['b'], 3)
t.eq(m['c'], 4)

-- Remove rc key from map.
m = {}
t.eq(m.size(), 0)
m[str(1)] = 123
m.remove('1')
t.eq(m.size(), 0)

-- contains()
m = {a=123}
t.eq(m.contains('a'), true)
t.eq(m.contains('b'), false)

-- get()
m = {a=123}
t.eq(m.get('a').?, 123)
t.assert(m.get('b') == none)

-- Force rehash.
type S:
    a str
    b int
m2 := Map[str, S]{}
for 0..10 |i|:
    m2[str(i)] = S{a=str(i), b=i}
for 0..10 |i|:
    t.eq(str(i), m2[str(i)].a)
    t.eq(i, m2[str(i)].b)

--cytest: pass