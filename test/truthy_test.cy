import t 'test'

-- Non zero number evaluates to true.
try t.eq(bool(123), true)
try t.eq(bool(-123), true)
try t.eq(bool(0), false)

-- String evaluates to true if not empty.
try t.eq(bool('cyber'), true)
try t.eq(bool(''), false)

-- Heap objects evaluate to true.
try t.eq(bool({}), true)
try t.eq(bool([]), true)
object S:
  a
try t.eq(bool(S{a: 0}), true)

-- none evaluates to false
try t.eq(bool(none), false)