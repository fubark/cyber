import test

var a = true
test.eq(a, true)

a = false
test.eq(a, false)

test.assert(true != false)

--cytest: pass