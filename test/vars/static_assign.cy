import t 'test'

var Root.a = 123

-- Assignment to a static variable.
a = 234
var f = func():
    t.eq(a, 234)
try f()

-- Assignment with same name in nested block does not write to static var.
a = 123
f = func():
    var a = 234
    t.eq(a, 234)
try f()
t.eq(a, 123)

-- Assignment to static variable inside nested block.
a = 123
f = func():
    a = 234
    t.eq(a, 234)
try f()
t.eq(a, 234)

-- Subsequent assignment to static variable inside nested block.
a = 123
f = func():
    a = 234
    a = 345
    t.eq(a, 345)
try f()
t.eq(a, 345)

-- Assignment to a static variable before it is declared.
f = func():
    b = 234
    t.eq(b, 234)
try f()
t.eq(b, 234)
var Root.b = 123

-- Operator assignment to a static variable.
a = 123
a += 321
t.eq(a, 444)

--cytest: pass