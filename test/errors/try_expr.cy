-- Copyright (c) 2023 Cyber (See LICENSE)

use t 'test'

fn fail() dyn:
    throw error.Fail

-- Non-error.
t.eq(try 1, 1)

-- Non-error inside function.
fn foo() dyn:
    return try 1
t.eq(foo(), 1)

-- Non-error inside assignment function assignment.
fn foo2() dyn:
    var val = try 1
    return val
t.eq(foo2(), 1)

-- Non-error rc value inside function.
fn foo3() dyn:
    return try 'abc'
t.eq(foo3() as string, 'abc')

-- Non-error rc value assignment inside function.
fn foo4() dyn:
    var val = try 'abc'
    return val
t.eq(foo4() as string, 'abc')

-- Caught error.
var res = try fail()
t.eq(res, error.Fail)

-- Caught error inside function.
fn foo5() dyn:
    return try fail()
t.eq(foo5(), error.Fail)

-- Error value assignment inside function. Returns from function.
fn foo6() dyn:
    var val = try fail()
    return val
t.eq(foo6(), error.Fail)

--cytest: pass