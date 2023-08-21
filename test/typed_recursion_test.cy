import t 'test'

func fib(n int) int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)
var res = fib(6)
t.eq(res, int(8))
t.eq(typesym(res), #int)