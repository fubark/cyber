use test

-- Boxing primitive.
var a any = 123
test.eq(a, 123)

-- Boxing primitive expr.
var b = 2 | 1
test.eq(b, 3) 

--cytest: pass