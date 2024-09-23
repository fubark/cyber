use t 'test'

-- Overload by number of params.
fn foo() int:
    return 2 + 2

fn foo(n int) int:
    return 2 + n

fn foo(n int, m int) int:
    return n * m

t.eq(foo(), 4)
t.eq(foo(10), 12)
t.eq(foo(3, 5), 15)

--cytest: pass