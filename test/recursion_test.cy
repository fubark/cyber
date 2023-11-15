import t 'test'

func foo(n):
    if n is 0:
        return 0
    return n + foo(n-1)
t.eq(foo(10), 55)

-- Recursion with long lived object.
type S object:
    var n
func foo2(o):
    if o.n is 0:
        return 0
    var n = o.n
    o.n = o.n - 1
    return n + foo2(o)
t.eq(foo2([S n: 10]), 55) 

-- Recursion with new objects.
func foo3(o):
    if o.n is 0:
        return 0
    return o.n + foo3([S n: o.n - 1])
t.eq(foo3([S n: 10]), 55)