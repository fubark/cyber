use t 'test'

-- Overload by number of params.
func foo() int:
    return 2 + 2

func foo(n int) int:
    return 2 + n

func foo(n int, m int) int:
    return n * m

t.eq(foo(), 4)
t.eq(foo(10), 12)
t.eq(foo(3, 5), 15)

--cytest: pass