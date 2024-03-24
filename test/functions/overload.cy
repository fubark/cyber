import t 'test'

-- Overload by number of params.
func foo():
    return 2 + 2

func foo(n int):
    return 2 + n

func foo(n int, m int):
    return n * m

t.eq(foo(), 4)
t.eq(foo(10), 12)
t.eq(foo(3, 5), 15)

--cytest: pass