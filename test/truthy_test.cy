import t 'test'

try t.eq(bool(true), true)
try t.eq(bool(false), false)

-- Numbers including zero evaluates to true.
try t.eq(bool(123), true)
try t.eq(bool(-123), true)
try t.eq(bool(0), true)

-- Strings including the empty string evaluates to true.
try t.eq(bool('cyber'), true)
try t.eq(bool(''), true)

-- Heap objects evaluate to true.
try t.eq(bool({}), true)
try t.eq(bool([]), true)
object S:
  a
try t.eq(bool(S{a: 0}), true)

-- none evaluates to false
try t.eq(bool(none), false)