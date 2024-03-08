import t 'test'

--|
--| float
--|

func staticPrim(a float):
	return a == 123.0

type PrimType:
	a float

-- Call static function with literal.
t.eq(staticPrim(123.0), true)

-- Call static function infer type.
t.eq(staticPrim(123), true)

-- Call static function with var.
my n = 123.0
t.eq(staticPrim(n), true)

-- Call static function from cast.
n = t.erase(123.0)
t.eq(staticPrim(n as float), true)

-- Call static function with object access.
var o = [PrimType a: 123.0]
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

--|
--| pointer.
--|
func fooPointer(a pointer):
  return a.addr() == 123

-- From var.
var ptr = pointer(123)
t.eq(fooPointer(ptr), true)

-- Cast erased type.
ptr = t.erase(pointer(123))
t.eq(fooPointer(pointer(ptr)), true)

--|
--| String.
--|
func fooString(a String):
  return a == 'true'

-- Literal.
t.eq(fooString('true'), true)

-- From var.
var str = 'true'
t.eq(fooString(str), true)

-- Cast erased type.
str = t.erase('true')
t.eq(fooString(String(str)), true)

-- bool.
func fooBool(a bool):
  return a

-- Literal.
t.eq(fooBool(true), true)

-- From var.
var b = true
t.eq(fooBool(b), true)

-- Cast erased type.
b = t.erase(true)
t.eq(fooBool(bool(b)), true)

--|
--| Map
--|
func fooMap(a Map):
    return a['a'] == 123

-- Literal.
t.eq(fooMap([a: 123]), true)

-- From var.
var map = [a: 123]
t.eq(fooMap(map), true)

-- Cast erased type.
map = t.erase([a: 123])
t.eq(fooMap(map as Map), true)

--|
--| List
--|
func fooList(a List):
    return a[0] == 123

-- Literal.
t.eq(fooList([123]), true)

-- From var.
var list = [123]
t.eq(fooList(list), true)

-- Cast erased type.
list = t.erase([123])
t.eq(fooList(list as List), true)

--|
--| symbol
--|
func fooSymbol(a symbol):
    return a == .sometag

-- Literal.
t.eq(fooSymbol(.sometag), true)

-- From var.
var tag = .sometag
t.eq(fooSymbol(tag), true)

-- Cast erased type.
tag = t.erase(.sometag)
t.eq(fooSymbol(tag as symbol), true)

--cytest: pass