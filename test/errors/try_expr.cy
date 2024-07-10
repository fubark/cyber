-- Copyright (c) 2023 Cyber (See LICENSE)

use t 'test'

func fail():
    throw error.Fail

-- Non-error.
t.eq(try 1, 1)

-- Non-error inside function.
func foo():
    return try 1
t.eq(foo(), 1)

-- Non-error inside assignment function assignment.
func foo2():
    var val = try 1
    return val
t.eq(foo2(), 1)

-- Non-error rc value inside function.
func foo3():
    return try []
t.eqList(foo3() as List[dyn], [])

-- Non-error rc value assignment inside function.
func foo4():
    var val = try []
    return val
t.eqList(foo4() as List[dyn], [])

-- Caught error.
var res = try fail()
t.eq(res, error.Fail)

-- Caught error inside function.
func foo5():
    return try fail()
t.eq(foo5(), error.Fail)

-- Error value assignment inside function. Returns from function.
func foo6():
    var val = try fail()
    return val
t.eq(foo6(), error.Fail)

--cytest: pass