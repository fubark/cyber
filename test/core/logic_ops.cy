use t 'test'

-- or operator
t.eq(false or false, false)
t.eq(false or true, true)
t.eq('cyber' == 'cyber' or false, true)
t.eq(false or 'cyber' == 'cyber', true)
-- or assigned to local.
var res = false or 'cyber' == 'cyber'
t.eq(res, true)
-- or with retained local.
t.eq(res or 'cyber' == 'cyber', true)

var .called = false
fn foo() bool:
    called = true
    return true

-- If first `or` operand evaluates to true, the second expression is not evaluated
-- and the first operand is returned.
called = false
t.eq(true or foo(), true)
t.eq(called, false)

-- If first `or` operand evaluates to false, the second expression is evaluated and returned.
t.eq(false or true, true)
t.eq(false and true, false)
t.eq(true and true, true)

-- First false skips second operand evaluation.
called = false
t.eq(false and foo(), false)
t.eq(called, false)

-- If first `and` operand evaluates to false, the second expression is not evaluated
-- and the first operand is returned.
called = false
t.eq(false and foo(), false)
t.eq(called, false)

-- If first `and` operand evaluates to true, the second expression is evaluated and returned.
t.eq(true and true, true)

-- not operator
t.eq(not false, true)
t.eq(not true, false)
t.eq(!false, true)
t.eq(!true, false)

--cytest: pass