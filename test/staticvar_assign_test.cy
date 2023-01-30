-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

var a = 123

-- Assignment to a static variable.
a = 234
func foo():
    try t.eq(a, 234)
try foo()

-- Assignment with same name in nested block does not write to static var.
a = 123
func foo2():
    a = 234
    try t.eq(a, 234)
try foo2()
try t.eq(a, 123)

-- Assignment to static variable inside nested block.
a = 123
func foo3():
    static a = 234
    try t.eq(a, 234)
try foo3()
try t.eq(a, 234)

-- Subsequent assignment to static variable inside nested block.
a = 123
func foo4():
    static a = 234
    a = 345
    try t.eq(a, 345)
try foo4()
try t.eq(a, 345)

-- Assignment to a static variable before it is declared.
func foo5():
    static b = 234
    try t.eq(b, 234)
try foo5()
try t.eq(b, 234)
var b = 123

-- Operator assignment to a static variable.
a = 123
a += 321
try t.eq(a, 444)