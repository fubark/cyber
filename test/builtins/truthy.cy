import t 'test'

t.eq(bool(true), true)
t.eq(bool(false), false)

-- Numbers including zero evaluates to true.
t.eq(bool(123), true)
t.eq(bool(-123), true)
t.eq(bool(0), true)

-- Strings including the empty string evaluates to true.
t.eq(bool('cyber'), true)
t.eq(bool(''), true)

-- Heap objects evaluate to true.
t.eq(bool([:]), true)
t.eq(bool([]), true)
type S object:
    var a
t.eq(bool([S a: 0]), true)

-- none evaluates to false
t.eq(bool(none), false)

--cytest: pass