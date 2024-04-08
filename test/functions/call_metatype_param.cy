use t 'test'

func foo(a metatype):
    return a == float

-- Literal.
t.eq(foo(float), true)
        
-- From var.
var mt = float
t.eq(foo(mt), true)

-- Cast erased type.
mt = t.erase(float)
t.eq(foo(mt as metatype), true)

--cytest: pass