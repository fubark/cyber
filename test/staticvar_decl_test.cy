-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Reading from a static variable before it is declared.
try t.eq(a, 123)

var a = 123

-- Reading from a static variable.
try t.eq(a, 123)

-- Invoke as function.
var a1 = func():
    return 123
var a2 = func(a):
    return a + 1
var a3 = func(a, b):
    return a + b
try t.eq(a1(), 123)
try t.eq(a2(123), 124)
try t.eq(a3(123, 321), 444)

-- Invoke method.
var a4 = [1, 2, 3]
try t.eq(a4.len(), 3)
a4.append(4)
try t.eq(a4.len(), 4)

-- Using object initializer.
object Object:
    foo
var a5 = Object{ foo: 123 }
try t.eq(a5.foo, 123)