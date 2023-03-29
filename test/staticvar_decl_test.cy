-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Reading from a static variable before it is declared.
t.eq(a, 123)

var a: 123

-- Reading from a static variable.
t.eq(a, 123)

-- Invoke as function.
var a1: func():
    return 123
var a2: func(a):
    return a + 1
var a3: func(a, b):
    return a + b
t.eq(a1(), 123)
t.eq(a2(123), 124)
t.eq(a3(123, 321), 444)

-- Invoke method.
var a4: [1, 2, 3]
t.eq(a4.len(), 3)
a4.append(4)
t.eq(a4.len(), 4)

-- Using object initializer.
type Object object:
    foo
var a5: Object{ foo: 123 }
t.eq(a5.foo, 123)