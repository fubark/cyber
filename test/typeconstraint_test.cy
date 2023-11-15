import t 'test'

--|
--| Generic primitive type.
--|

func staticPrim(a int):
	return a == 123

type PrimType object:
	var a int

-- Call static function with literal.
t.eq(staticPrim(123), true)

-- Call static function with var.
my n = 123
t.eq(staticPrim(n), true)

-- Call static function from cast.
n = t.erase(123)
t.eq(staticPrim(n as int), true)

-- Call static function with object access.
var o = [PrimType a: 123]
t.eq(staticPrim(o.a), true)

--|
--| int
--|
func fooInt(a int):
    return a == 123

-- Literal.
t.eq(fooInt(123), true)
        
-- From var.
n = 123
t.eq(fooInt(n), true)

-- Cast erased type.
n = t.erase(123)
t.eq(fooInt(n as int), true)

--|
--| none.
--|
func fooNone(a none):
    return a == none

-- Literal.
t.eq(fooNone(none), true)
        
-- From var.
n = none
t.eq(fooNone(n), true)
