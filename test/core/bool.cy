use t 'test'

var a = true
t.eq(a, true)

a = false
t.eq(a, false)

t.assert(true != false)

t.eq(bool(true), true)
t.eq(bool(false), false)

-- Integers evaluate to true except 0.
t.eq(bool(123), true)
t.eq(bool(-123), true)
t.eq(bool(0), false)

-- Floats evaluate to true except 0.
t.eq(bool(123.0), true)
t.eq(bool(-123.0), true)
t.eq(bool(0.0), false)

-- Strings evaluate to true except the empty string.
t.eq(bool('cyber'), true)
t.eq(bool(''), false)

-- Heap objects evaluate to true.
t.eq(bool({}), true)
t.eq(bool([]), true)
type S:
    a any
t.eq(bool(S{a: 0}), true)

--cytest: pass