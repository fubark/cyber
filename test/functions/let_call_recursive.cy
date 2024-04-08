use t 'test'

let foo(n):
    if n == 0:
        return 0
    return n + foo(n-1)
t.eq(foo(10), 55)

-- Recursion with long lived object.
type S:
    n any
let foo2(o):
    if o.n == 0:
        return 0
    let n = o.n
    o.n = o.n - 1
    return n + foo2(o)
t.eq(foo2(S{n: 10}), 55) 

-- Recursion with new objects.
let foo3(o):
    if o.n == 0:
        return 0
    return o.n + foo3(S{n: o.n - 1})
t.eq(foo3(S{n: 10}), 55)

--cytest: pass