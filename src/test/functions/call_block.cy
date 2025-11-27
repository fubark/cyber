use test

global n int = 0

type Fn1 = fn() -> void
type Fn2 = fn() -> int

fn foo(func Fn1):
    func()

fn fooRet(func Fn2) -> int:
    return func()

fn fooRet2(a int, func Fn2) -> int:
    return a + func()

-- Expression stmt.
n = 123
foo(|_|):
    n += 1
test.eq(124, n)

-- Decl stmt.
n = 123
a := fooRet(|_|):
    return n + 1
test.eq(a, 124)
a2 := fooRet2(10, |_|):
    return n
test.eq(a2, 133)

-- Decl stmt, ending expr.
n = 123
b := 1 + fooRet(|_|):
    return n + 1
test.eq(b, 125)
b2 := 1 + fooRet2(10, |_|):
    return n
test.eq(b2, 134)

-- Assign stmt.
n = 123
a = fooRet(|_|):
    return n + 1
test.eq(a, 124)
a = fooRet2(10, |_|):
    return n
test.eq(a, 133)

-- Assign stmt, ending expr.
n = 123
b = 1 + fooRet(|_|):
    return n + 1
test.eq(b, 125)
b = 1 + fooRet2(10, |_|):
    return n
test.eq(b, 134)

-- TODO: Named arguments.

-- TODO: Declare lambda params.

--cytest: pass