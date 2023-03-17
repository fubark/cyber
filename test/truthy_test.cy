import t 'test'

try t.eq(boolean(true), true)
try t.eq(boolean(false), false)

-- Numbers including zero evaluates to true.
try t.eq(boolean(123), true)
try t.eq(boolean(-123), true)
try t.eq(boolean(0), true)

-- Strings including the empty string evaluates to true.
try t.eq(boolean('cyber'), true)
try t.eq(boolean(''), true)

-- Heap objects evaluate to true.
try t.eq(boolean({}), true)
try t.eq(boolean([]), true)
object S:
  a
try t.eq(boolean(S{a: 0}), true)

-- none evaluates to false
try t.eq(boolean(none), false)