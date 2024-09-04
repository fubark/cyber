use t 'test'

func foo(a Fiber) bool:
    return type(a) == Fiber

func start() dyn:
    pass

-- Literal.
t.eq(foo(coinit(start)), true)
        
-- From var.
var f = coinit(start)
t.eq(foo(f), true)

-- Cast erased type.
dyn f2 = coinit(start)
t.eq(foo(f2 as Fiber), true)

--cytest: pass