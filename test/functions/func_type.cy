use test

-- With no params, no return type.
type Fn1 -> fn()
fn foo():
    pass

var func fn() = foo
var func2 Fn1 = foo
test.eq(func, foo)
test.eq(func2, foo)

-- With param, no return type.
type Fn2 -> fn(int)
fn foo2(a int):
    pass

var func3 fn(int) = foo2
var func4 Fn2 = foo2
test.eq(func3, foo2)
test.eq(func4, foo2)

-- With param and return type.
type Fn3 -> fn(int) int
fn foo3(a int) int:
    return a

var func5 fn(int) int = foo3
var func6 Fn3 = foo3
test.eq(func5, foo3)
test.eq(func6, foo3)
test.eq(func6(10), 10)

--cytest: pass