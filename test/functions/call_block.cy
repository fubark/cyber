use test

var .n int = 0

func foo(fn func()):
    fn()

func fooRet(fn func() int) int:
    return fn()

-- Expression stmt.
n = 123
foo() => ():
    n += 1
test.eq(n, 124)

-- Decl stmt.
n = 123
var a = fooRet() => ():
    return n + 1
test.eq(a, 124)

-- Decl stmt, ending expr.
n = 123
var b = 1 + fooRet() => ():
    return n + 1
test.eq(b, 125)

-- Assign stmt.
n = 123
a = fooRet() => ():
    return n + 1
test.eq(a, 124)

-- Assign stmt, ending expr.
n = 123
b = 1 + fooRet() => ():
    return n + 1
test.eq(b, 125)

-- TODO: Named arguments.

-- TODO: Declare lambda params.

--cytest: pass