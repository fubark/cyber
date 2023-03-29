import t 'test'

t.eq(boolean(true), true)
t.eq(boolean(false), false)

-- Numbers including zero evaluates to true.
t.eq(boolean(123), true)
t.eq(boolean(-123), true)
t.eq(boolean(0), true)

-- Strings including the empty string evaluates to true.
t.eq(boolean('cyber'), true)
t.eq(boolean(''), true)

-- Heap objects evaluate to true.
t.eq(boolean({}), true)
t.eq(boolean([]), true)
type S object:
  a
t.eq(boolean(S{a: 0}), true)

-- none evaluates to false
t.eq(boolean(none), false)