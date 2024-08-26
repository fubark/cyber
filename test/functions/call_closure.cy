use t 'test'

-- Closure read over number in main block.
var a = 123
var foo = func () => a
t.eq(foo(), 123)

-- Closure write over number in main block.
var a2 = 123
var foo2 = func():
    a2 = 234
foo2()
t.eq(a2, 234)

-- Closure over local number in function.
var f = func() (Func() int):
    var a = 123
    return () => a
var fn = f()
t.eq(fn(), 123)

-- Closure over param number in function.
var f2 = func(a int) (Func() int):
    return () => a * 2
fn = f2(22)
t.eq(fn(), 44)

-- Closure over local number in function using a param.
var f3 = func() (Func(int) int):
    var a = 123
    return b => a + b
var fn2 = f3()
t.eq(fn2(1), 124)

-- Closure over local number in function using a param in parentheses.
f3 = func() (Func(int) int):
    var a = 123
    return (b) => a + b
fn2 = f3()
t.eq(fn2(1), 124)

-- Closure over local number in function using a multiple params.
var f4 = func() (Func(int, int) int):
    var a = 123
    return (b, c) => a + b + c
var fn3 = f4()
t.eq(fn3(1, 2), 126)

-- Closure over local retained object in function.
var f5 = func() (Func() dyn):
    var a = {123}
    return () => a[0]
var fn4 = f5()
t.eq(fn4(), 123)

-- Closure with more than 3 captured vars forces allocation outside of object pool.
var a3 = 123
var b = 234
var c = 345
var d = 456
foo = () => a3 + b + c + d
t.eq(foo(), 1158)

-- Typed param.
if true:
    var b = 2
    var foo = func (a int) int:
        return a + b
    t.eq(foo(1), 3)

--cytest: pass