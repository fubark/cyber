use t 'test'

-- Throw statement.
fn fail() dyn:
    throw error.Fail
var res = try fail()
t.eq(res, error.Fail)

-- Throw expression.
fn fail2() bool:
    return false or throw error.Fail
res = try fail2()
t.eq(res, error.Fail)

--cytest: pass