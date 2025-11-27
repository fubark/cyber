use t 'test'

global a int = 123

-- Assignment to a static variable.
a = 234
f := fn():
    t.eq(a, 234)
f()

-- Assignment with same name in nested block does not write to static var.
a = 123
f = |_|:
    a := 234
    t.eq(a, 234)
f()
t.eq(a, 123)

-- Assignment to static variable inside nested block.
a = 123
f = |_|:
    a = 234
    t.eq(a, 234)
f()
t.eq(a, 234)

-- Subsequent assignment to static variable inside nested block.
a = 123
f = |_|:
    a = 234
    a = 345
    t.eq(a, 345)
f()
t.eq(a, 345)

-- Assignment to a static variable before it is declared.
f = |_|:
    b = 234
    t.eq(b, 234)
f()
t.eq(b, 234)
global b int = 123

-- Operator assignment to a static variable.
a = 123
a += 321
t.eq(a, 444)

--cytest: pass