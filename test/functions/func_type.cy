use test

-- With no params, no return type.
type Fn -> func()
func foo():
    pass

var fn func() = foo
var fn2 Fn = foo
test.eq(fn, foo)
test.eq(fn2, foo)

-- With param, no return type.
type Fn2 -> func(int)
func foo2(a int):
    pass

var fn3 func(int) = foo2
var fn4 Fn2 = foo2
test.eq(fn3, foo2)
test.eq(fn4, foo2)

-- With param and return type.
type Fn3 -> func(int) int
func foo3(a int) int:
    return a

var fn5 func(int) int = foo3
var fn6 Fn3 = foo3
test.eq(fn5, foo3)
test.eq(fn6, foo3)
test.eq(fn6(10), 10)

--cytest: pass