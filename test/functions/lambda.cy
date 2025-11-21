use t 'test'

-- Pass lambda as arg.
type F1 = fn(int) -> int
fn call(f F1) -> int:
    return f(14)
t.eq(15, call(|a| a + 1))

-- Using parentheses.
type F2 = fn() -> int
type S:
    a F2
s := S{a=|_| 4}
t.eq((s.a)(), 4)

-- Invoking lambda temp.
t.eq(15, (fn(a int) => a + 1)(14))

-- Expr body, no params.
foo := fn() => 2 + 2
t.eq(foo(), 4)

-- No params.
tfoo := fn() -> int:
    return 2 + 2
t.eq(tfoo(), 4)

-- Expr body, one param.
foo2 := fn(a int) => a + 1
t.eq(foo2(10), 11)

-- One param.
tfoo2 := fn(a int) -> int:
    return a + 1
t.eq(tfoo2(10), 11)

-- Expr body, multiple params.
foo3 := fn(bar int, inc int) => bar + inc
t.eq(foo3(20, 10), 30)

-- Multiple params.
tfoo3 := fn(bar int, inc int) -> int:
    return bar + inc
t.eq(tfoo3(20, 10), 30)

-- Single line block.
tfoo = fn() -> int: return 2 + 2
t.eq(tfoo(), 4)

--cytest: pass