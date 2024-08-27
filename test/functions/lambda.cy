use t 'test'

-- Pass lambda as arg.
func call(f func(int) int) int:
    return f(14)
t.eq(call(a => a + 1), 15)

-- Using parentheses.
var m = {a=func() => 4}
t.eq((m.a)(), 4)

-- Invoking lambda temp.
t.eq((func(a int) => a + 1)(14), 15)

-- Expr body, no params.
var foo = func() => 2 + 2
t.eq(foo(), 4)

-- No params.
var tfoo = func() int:
    return 2 + 2
t.eq(tfoo(), 4)

-- Expr body, one param.
var foo2 = func(a int) => a + 1
t.eq(foo2(10), 11)

-- One param.
var tfoo2 = func(a int) int:
    return a + 1
t.eq(tfoo2(10), 11)

-- Expr body, multiple params.
var foo3 = func(bar int, inc int) => bar + inc
t.eq(foo3(20, 10), 30)

-- Multiple params.
var tfoo3 = func(bar int, inc int) int:
    return bar + inc
t.eq(tfoo3(20, 10), 30)

-- Single line block.
tfoo = func() int: return 2 + 2
t.eq(tfoo(), 4)

--cytest: pass