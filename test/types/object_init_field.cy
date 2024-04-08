use test

type S:
    x float

-- Exact field type.
var s = S{x: 1.23}
test.eq(s.x, 1.23)

-- Infer field type.
s = S{x: 123}
test.eq(s.x, 123.0)

--cytest: pass