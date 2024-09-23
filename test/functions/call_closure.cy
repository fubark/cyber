use t 'test'

-- Closure read over number in main block.
var a = 123
var foo = fn() => a
t.eq(foo(), 123)

-- Closure write over number in main block.
var a2 = 123
var foo2 = fn():
    a2 = 234
foo2()
t.eq(a2, 234)

-- Closure over local number in function.
var f = fn() (Fn() int):
    var a = 123
    return () => a
var func = f()
t.eq(func(), 123)

-- Closure over param number in function.
var f2 = fn(a int) (Fn() int):
    return () => a * 2
func = f2(22)
t.eq(func(), 44)

-- Closure over local number in function using a param.
var f3 = fn() (Fn(int) int):
    var a = 123
    return b => a + b
var fn2 = f3()
t.eq(fn2(1), 124)

-- Closure over local number in function using a param in parentheses.
f3 = fn() (Fn(int) int):
    var a = 123
    return (b) => a + b
fn2 = f3()
t.eq(fn2(1), 124)

-- Closure over local number in function using a multiple params.
var f4 = fn() (Fn(int, int) int):
    var a = 123
    return (b, c) => a + b + c
var fn3 = f4()
t.eq(fn3(1, 2), 126)

-- Closure over local retained object in function.
var f5 = fn() (Fn() dyn):
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
    var foo = fn(a int) int:
        return a + b
    t.eq(foo(1), 3)

--cytest: pass