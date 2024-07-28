use test

var o = {}

-- Setting missing field.
o.c = 345
test.eq(o.c, 345)

-- Initialize with record pairs.
o = {a=123, b='abc'}
test.eq(o.a, 123)
test.eq(o.b, 'abc')

-- Indexing.
var key = 'a'
test.eq(o[key], 123)
o[key] = 999
test.eq(o[key], 999)

-- Indexing with non-string key.
o[10] = {1, 2, 3}
test.eq(o[10].len(), 3)

--cytest: pass