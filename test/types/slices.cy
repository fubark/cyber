use test

var a = 123
var ptr = &a
var slice = ptr[0..1]

-- Slice index.
test.eq(slice[0], 123)

-- Slice len.
test.eq(slice.len(), 1)

-- Slice set index.
slice[0] = 234
test.eq(slice[0], 234)

--cytest: pass