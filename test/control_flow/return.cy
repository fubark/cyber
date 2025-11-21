use t 'test'

-- if.
fn a(v int) -> int:
    if v == 1:
        return 10
    return 20
t.eq(a(1), 10)
t.eq(a(2), 20)

-- else.
fn a2(v int) -> int:
    if v == 1:
        return 10
    else:
        return 20
t.eq(a2(1), 10)
t.eq(a2(2), 20)

-- else if.
fn a3(v int) -> int:
    if v == 1:
        return 10
    else v == 2:
        return 20
    else:
        return 30
t.eq(a3(1), 10)
t.eq(a3(2), 20)
t.eq(a3(3), 30)

-- return multi-line lambda
type Fn1 = fn() -> int
fn foo() -> Fn1:
    return fn() -> int:
        return 123
t.eq(foo()(), 123)

--cytest: pass