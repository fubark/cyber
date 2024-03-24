import test

type S:
    a float

var o = S{a: 123.0}

-- Exact type.
o.a = 234.0
test.eq(o.a, 234.0)

-- Inferred type.
o.a = 345
test.eq(o.a, 345.0)

-- Op set field inferring rhs type.
o.a = 1
o.a += 234
test.eq(o.a, 235.0)

--cytest: pass