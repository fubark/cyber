use t 'test'

-- Untyped, shorthand, no params.
var foo = () => 2 + 2
t.eq(foo(), 4)

-- Untyped, shorthand, one param.
foo = a => a + 1
t.eq(foo(10), 11)

-- Untyped, shorthand, multiple params.
foo = (bar, inc) => bar + inc
t.eq(foo(20, 10), 30)

-- Untyped, no params.
foo = ():
    return 3
t.eq(foo(), 3)

-- Untyped, one param.
foo = (a):
    return a + 3
t.eq(foo(1), 4)

-- Untyped, multiple params.
foo = (a, b):
    return a + b
t.eq(foo(10, 11), 21)

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
foo = func ():
    return 2 + 2
t.eq(foo(), 4)

-- Typed, one param.
foo = func (a int):
    return a + 1
t.eq(foo(10), 11)

-- Typed, multiple params.
foo = func (bar int, inc int):
    return bar + inc
t.eq(foo(20, 10), 30)

-- Typed, single line block.
foo = func (): return 2 + 2
t.eq(foo(), 4)

--cytest: pass