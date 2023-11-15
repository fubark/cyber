import t 'test'

func fail():
    throw error.Fail

func happy(a):
    return a

-- Returns result.
var res = try 1 catch 0
t.eq(res, 1)

-- Top expr fails, returns default value.
res = try fail() catch 0
t.eq(res, 0)

-- Retained catch value.
res = try fail() catch 'retained'
t.eq(res, 'retained')

-- Sub expr fails, returns default value.
res = try happy(fail()) catch 0
t.eq(res, 0)
