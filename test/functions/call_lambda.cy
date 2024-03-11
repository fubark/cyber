import t 'test'

-- No params.
var foo = () => 2 + 2
t.eq(foo(), 4)

-- One param.
foo = a => a + 1
t.eq(foo(10), 11)

-- Multiple params.
foo = (bar, inc) => bar + inc
t.eq(foo(20, 10), 30)

-- Pass lambda as arg.
func call(f dynamic):
    return f(14)
t.eq(call(a => a + 1), 15)

-- Using parentheses.
var m = [ a: () => 4 ]
t.eq((m.a)(), 4)

-- Invoking lambda temp.
t.eq((a => a + 1)(14), 15)

-- Multi-line lambda, no params.
foo = func ():
    return 2 + 2
t.eq(foo(), 4)

-- Multi-line lambda, one param.
foo = func (a int):
    return a + 1
t.eq(foo(10), 11)

-- Multi-line lambda, multiple params.
foo = func (bar int, inc int):
    return bar + inc
t.eq(foo(20, 10), 30)

-- Multi-line lambda, single line block.
foo = func (): return 2 + 2
t.eq(foo(), 4)

-- Typed.
foo = func (a int):
    return a
t.eq(foo(1), 1)

--cytest: pass