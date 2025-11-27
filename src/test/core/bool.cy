use test

a := true
test.eq(true, a)

a = false
test.eq(false, a)

test.assert(true != false)

-- Integers evaluate to true except 0.
test.eq(true, bool(123))
test.eq(true, bool(-123))
test.eq(false, bool(0))

-- Floats evaluate to true except 0.
test.eq(true, bool(123.0))
test.eq(true, bool(-123.0))
test.eq(false, bool(0.0))

-- Strings evaluate to true except the empty string.
test.eq(true, bool('true'))
test.eq(false, bool(''))

--cytest: pass