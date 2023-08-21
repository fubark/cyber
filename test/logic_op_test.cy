import t 'test'

-- or operator
t.eq(false or false, false)
t.eq(false or true, true)
t.eq('cyber' or false, 'cyber')

-- If first `or` operand evaluates to true, the second expression is not evaluated
-- and the first operand is returned.
var a = none
t.eq(123 or a.foo, 123)

-- If first `or` operand evaluates to false, the second expression is evaluated and returned.
a = 123
t.eq(false or a, 123)
t.eq(false and true, false)
t.eq(true and true, true)

-- First false skips second operand evaluation.
var called: false
func foo():
  called = true
t.eq(false and foo(), false)
t.eq(called, false)

-- If first `and` operand evaluates to false, the second expression is not evaluated
-- and the first operand is returned.
a = none
t.eq(false and a.foo, false)

-- If first `and` operand evaluates to true, the second expression is evaluated and returned.
t.eq(123 and 234, 234)

-- not operator
t.eq(not false, true)
t.eq(not true, false)
t.eq(!false, true)
t.eq(!true, false)