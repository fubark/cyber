use test

var .n int = 0

fn foo(func fn()):
    func()

fn fooRet(func fn() int) int:
    return func()

fn fooRet2(a int, func fn() int) int:
    return a + func()

-- Expression stmt.
n = 123
foo(():
    n += 1
test.eq(n, 124)

-- Decl stmt.
n = 123
var a = fooRet(():
    return n + 1
test.eq(a, 124)
var a2 = fooRet2(10, ():
    return n
test.eq(a2, 133)

-- Decl stmt, ending expr.
n = 123
var b = 1 + fooRet(():
    return n + 1
test.eq(b, 125)
var b2 = 1 + fooRet2(10, ():
    return n
test.eq(b2, 134)

-- Assign stmt.
n = 123
a = fooRet(():
    return n + 1
test.eq(a, 124)
a = fooRet2(10, ():
    return n
test.eq(a, 133)

-- Assign stmt, ending expr.
n = 123
b = 1 + fooRet(():
    return n + 1
test.eq(b, 125)
b = 1 + fooRet2(10, ():
    return n
test.eq(b, 134)

-- TODO: Named arguments.

-- TODO: Declare lambda params.

--cytest: pass