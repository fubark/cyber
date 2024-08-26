use t 'test'

func foo(a metatype) bool:
    return a == float

-- Literal.
t.eq(foo(float), true)
        
-- From var.
var mt = metatype(float)
t.eq(foo(mt), true)

-- Cast erased type.
mt = t.erase(metatype(float))
t.eq(foo(mt as metatype), true)

--cytest: pass