use t 'test'

-- Reading from a static variable before it is declared.
t.eq(a, 123)

var .a = 123

-- Reading from a static variable.
t.eq(a, 123)

-- Type is inferred from initializer.
t.eq(typeof(a), int)

-- Invoke as function.
var .a1 = func() int:
    return 123
var .a2 = func(a int) int:
    return a + 1
var .a3 = func(a int, b int) int:
    return a + b
t.eq(a1(), 123)
t.eq(a2(123), 124)
t.eq(a3(123, 321), 444)

-- Invoke method.
var .a4 = {1, 2, 3}
t.eq(a4.len(), 3)
a4.append(4)
t.eq(a4.len(), 4)

-- Using object initializer.
type Object:
    foo any
var .a5 = Object{foo=123}
t.eq(a5.foo, 123)

-- Type specifier.
var .a6 float = 123.0
t.eq(a6, 123.0)

-- Type specifier infer type.
var .a7 float = 123
t.eq(a7, 123.0)

-- Declaration over using builtin module. 
var .print = 123
t.eq(print, 123)

--cytest: pass