import t 'test'

func fib(n int) int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)
res = fib(6)
try t.eq(res, int(8))
try t.eq(valtag(res), #int)