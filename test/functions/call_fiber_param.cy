use t 'test'

func foo(a Fiber) bool:
    return typeof(a) == Fiber

func start() dyn:
    pass

-- Literal.
t.eq(foo(coinit(start)), true)
        
-- From var.
var f = coinit(start)
t.eq(foo(f), true)

-- Cast erased type.
f = t.erase(coinit(start))
t.eq(foo(f as Fiber), true)

--cytest: pass