use t 'test'

fn foo(a type) bool:
    return a == float

-- Literal.
t.eq(foo(float), true)
        
-- From var.
var mt = float
t.eq(foo(mt), true)

-- Cast erased type.
dyn mt2 = float
t.eq(foo(mt2 as type), true)

--cytest: pass