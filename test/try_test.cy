-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Non-error.
t.eq(try 1, 1)

-- Non-error inside function.
func foo():
    try 1
    return 123
try t.eq(foo(), 123)

-- Non-error inside assignment function assignment.
func foo2():
    val = try 1
    return 123
try t.eq(foo2(), 123)

-- Non-error rc value inside function.
func foo3():
    try []
    return 123
try t.eq(foo3(), 123)

-- Non-error rc value assignment inside function.
func foo4():
    val = try []
    return 123
try t.eq(foo4(), 123)

-- Error value inside function. Returns from function.
func foo5():
    try error.boom
    return 123
try t.eq(foo5(), error.boom)

-- Error value assignment inside function. Returns from function.
func foo6():
    val = try error.boom
    return 123
try t.eq(foo6(), error.boom)