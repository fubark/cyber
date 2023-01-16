import t 'test'

-- or operator
try t.eq(false or false, false)
try t.eq(false or true, true)
try t.eq('cyber' or false, 'cyber')

-- If first `or` operand evaluates to true, the second expression is not evaluated
-- and the first operand is returned.
a = none
try t.eq(123 or a.foo, 123)

-- If first `or` operand evaluates to false, the second expression is evaluated and returned.
a = 123
try t.eq(0 or a, 123)
try t.eq(false and true, false)
try t.eq(true and true, true)

-- First false skips second operand evaluation.
called = false
func foo():
  called = true
try t.eq(false and foo(), false)
try t.eq(called, false)

-- If first `and` operand evaluates to false, the second expression is not evaluated
-- and the first operand is returned
a = none
try t.eq(0 and a.foo, 0)

-- If first `and` operand evaluates to true, the second expression is evaluated and returned.
try t.eq(123 and 234, 234)

-- not operator
try t.eq(not false, true)
try t.eq(not true, false)
try t.eq(!false, true)
try t.eq(!true, false)