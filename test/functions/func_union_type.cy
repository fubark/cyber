use test

func foo(a int) int:
    return a

-- func union can be assigned with func sym.
var fn Func(int) int = foo
test.eq(fn(123), 123)

-- func union can be assigned with lamda.
fn = func(a int) int:
    return a * 2
test.eq(fn(10), 20)

-- func union can be assigned with closure.
var a = 123
fn = func(b int) int:
    return a + b
test.eq(fn(1), 124)

--cytest: pass