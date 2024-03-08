import test

func foo(n):
    return n + 1

-- Expression statement.
foo 123

-- Assign statement.
var a = foo 123
test.eq(a, 124)

func foo2(n, m):
    return n + m

-- Multiple arguments
a = foo2 123, 321
test.eq(a, 444)

--cytest: pass