use t 'test'

-- Closure read over number in main block.
var a = 123
var foo = () => a
t.eq(foo(), 123)

-- Closure write over number in main block.
var a2 = 123
foo = func():
    a2 = 234
foo()
t.eq(a2, 234)

-- Closure over local number in function.
var f = func():
    var a = 123
    return () => a
var fn = f()
t.eq(fn(), 123)

-- Closure over param number in function.
f = func(a int):
    return () => a * 2
fn = f(22)
t.eq(fn(), 44)

-- Closure over local number in function using a param.
f = func():
    var a = 123
    return b => a + b
fn = f()
t.eq(fn(1), 124)

-- Closure over local number in function using a param in parentheses.
f = func():
    var a = 123
    return (b) => a + b
fn = f()
t.eq(fn(1), 124)

-- Closure over local number in function using a multiple params.
f = func():
    var a = 123
    return (b, c) => a + b + c
fn = f()
t.eq(fn(1, 2), 126)

-- Closure over local retained object in function.
f = func():
    var a = {123}
    return () => a[0]
fn = f()
t.eq(fn(), 123)

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
    var foo = func (a int):
        return a + b
    t.eq(foo(1), 3)

--cytest: pass