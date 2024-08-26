use t 'test'

-- Untyped, shorthand, no params.
var foo = () => 2 + 2
t.eq(foo(), 4)

-- Untyped, shorthand, one param.
var foo2 = a => a + 1
t.eq(foo2(10), 11)

-- Untyped, shorthand, multiple params.
var foo3 = (bar, inc) => bar + inc
t.eq(foo3(20, 10), 30)

-- Untyped, no params.
foo = ():
    return 3
t.eq(foo(), 3)

-- Untyped, one param.
foo2 = (a):
    return a + 3
t.eq(foo2(1), 4)

-- Untyped, multiple params.
foo3 = (a, b):
    return a + b
t.eq(foo3(10, 11), 21)

-- Pass lambda as arg.
func call(f dyn):
    return f(14)
t.eq(call(a => a + 1), 15)

-- Using parentheses.
var m = {a=() => 4}
t.eq((m.a)(), 4)

-- Invoking lambda temp.
t.eq((a => a + 1)(14), 15)

-- Typed, no params.
var tfoo = func ():
    return 2 + 2
t.eq(tfoo(), 4)

-- Typed, one param.
var tfoo2 = func (a int):
    return a + 1
t.eq(tfoo2(10), 11)

-- Typed, multiple params.
var tfoo3 = func (bar int, inc int):
    return bar + inc
t.eq(tfoo3(20, 10), 30)

-- Typed, single line block.
tfoo = func (): return 2 + 2
t.eq(tfoo(), 4)

--cytest: pass