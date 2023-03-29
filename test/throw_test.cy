import t 'test'

-- Throw statement.
func fail():
    throw error.Fail
res = try fail()
t.eq(res, error.Fail)

-- Throw expression.
func fail2():
    a = false or throw error.Fail
res = try fail2()
t.eq(res, error.Fail)