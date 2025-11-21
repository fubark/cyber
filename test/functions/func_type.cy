use test

-- With no params, no return type.
type Fn1 = fn()
fn foo():
    pass

func := as[Fn1] foo
test.eq(func, foo)

-- With param, no return type.
type Fn2 = fn(int)
fn foo2(a int):
    pass

func2 := as[Fn2] foo2
test.eq(func2, foo2)

-- With param and return type.
type Fn3 = fn(int) -> int
fn foo3(a int) -> int:
    return a

func3 := as[Fn3] foo3
test.eq(func3, foo3)
test.eq(func3(10), 10)

--cytest: pass