import t 'test'

--|
--| Generic primitive type.
--|

func staticPrim(a number):
	return a == 123
type PrimType object:
	a number

-- Call static function with literal.
t.eq(staticPrim(123), true)

-- Call static function with var.
n = 123
t.eq(staticPrim(n), true)

-- Call static function from cast.
n = t.erase(123)
t.eq(staticPrim(n as number), true)

-- Call static function with object access.
o = PrimType{ a: 123 }
t.eq(staticPrim(o.a), true)

--|
--| number
--|
func fooNumber(a number):
  return a == 123

-- Literal.
t.eq(fooNumber(123), true)
        
-- From var.
n = 123
t.eq(fooNumber(n), true)

-- Cast erased type.
n = t.erase(123)
t.eq(fooNumber(n as number), true)

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
