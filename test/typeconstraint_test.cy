import t 'test'

--|
--| Generic primitive type.
--|

func staticPrim(a float):
	return a == 123
type PrimType object:
	a float

-- Call static function with literal.
t.eq(staticPrim(123), true)

-- Call static function with var.
var n = 123
t.eq(staticPrim(n), true)

-- Call static function from cast.
n = t.erase(123)
t.eq(staticPrim(n as float), true)

-- Call static function with object access.
var o = PrimType{ a: 123 }
t.eq(staticPrim(o.a), true)

--|
--| float
--|
func fooFloat(a float):
  return a == 123

-- Literal.
t.eq(fooFloat(123), true)
        
-- From var.
n = 123
t.eq(fooFloat(n), true)

-- Cast erased type.
n = t.erase(123)
t.eq(fooFloat(n as float), true)

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
