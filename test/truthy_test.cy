import t 'test'

t.eq(toBoolean(true), true)
t.eq(toBoolean(false), false)

-- Numbers including zero evaluates to true.
t.eq(toBoolean(123), true)
t.eq(toBoolean(-123), true)
t.eq(toBoolean(0), true)

-- Strings including the empty string evaluates to true.
t.eq(toBoolean('cyber'), true)
t.eq(toBoolean(''), true)

-- Heap objects evaluate to true.
t.eq(toBoolean({}), true)
t.eq(toBoolean([]), true)
type S object:
  a
t.eq(toBoolean(S{a: 0}), true)

-- none evaluates to false
t.eq(toBoolean(none), false)