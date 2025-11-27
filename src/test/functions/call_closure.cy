use t 'test'

-- Closure read over number in main block.
a := ^123
foo := fn() => a.*
t.eq(foo(), 123)

-- Closure write over number in main block.
a2 := ^123
foo2 := fn():
    a2.* = 234
foo2()
t.eq(a2.*, 234)

-- Closure over local number in function.
type Fn1 = fn() -> int
f := fn() -> Func[Fn1]:
    a := ^123
    return |_| a.*
func := f()
t.eq(func(), 123)

-- Closure over param number in function.
f2 := fn(a ^int) -> Func[Fn1]:
    return |_| a.* * 2
func = f2(^22)
t.eq(func(), 44)

-- Closure over local number in function using a param.
type Fn2 = fn(int) -> int
f3 := fn() -> Func[Fn2]:
    a := ^123
    return |b| a.* + b
fn2 := f3()
t.eq(fn2(1), 124)

-- Closure over local number in function using a param in parentheses.
f3 = fn() -> Func[Fn2]:
    a := ^123
    return |b| a.* + b
fn2 = f3()
t.eq(fn2(1), 124)

-- Closure over local number in function using a multiple params.
type Fn3 = fn(int, int) -> int
f4 := fn() -> Func[Fn3]:
    a := ^123
    return |b, c| a.* + b + c
fn3 := f4()
t.eq(fn3(1, 2), 126)

-- Closure over object access.
type Foo:
    a int
type Fn4 = fn() -> int
f5 := fn() -> Func[Fn4]:
    a := ^Foo{a=123}
    return |_| a.a
fn4 := f5()
t.eq(123, fn4())

-- Closure with more than 3 captured vars forces allocation outside of object pool.
a3 := ^123
b := ^234
c := ^345
d := ^456
foo = |_| a3.* + b.* + c.* + d.*
t.eq(foo(), 1158)

-- Typed param.
if true:
    b := ^2
    foo := fn(a int) -> int:
        return a + b.*
    t.eq(foo(1), 3)

-- Allow capturing borrows if pinned closure.
local_a := 123
borrow_a := &local_a
stack_fn := fn() => borrow_a.*
t.eq(stack_fn(), 123)

-- -- Allow capturing locals if stack closure.
-- local_a := 123
-- stack_fn = () => local_a
-- t.eq(stack_fn(), 123)

--cytest: pass