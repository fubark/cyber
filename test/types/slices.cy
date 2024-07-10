use test

var a = 123
var ptr = &a
var slice = ptr[0..1]
test.eq(slice[0], 123)
test.eq(slice.len(), 1)

--cytest: pass