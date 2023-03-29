import t 'test'

func fail():
    throw error.Fail

func happy(a):
    return a

-- Returns result.
res = try 1 else 0
t.eq(res, 1)

-- Top expr fails, returns default value.
res = try fail() else 0
t.eq(res, 0)

-- Sub expr fails, returns default value.
res = try happy(fail()) else 0
t.eq(res, 0)