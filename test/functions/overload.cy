import t 'test'

func foo():
    return 2 + 2

func foo(n):
    return 2 + n

func foo(n, m):
    return n * m

t.eq(foo(), 4)
t.eq(foo(10), 12)
t.eq(foo(3, 5), 15)

--cytest: pass