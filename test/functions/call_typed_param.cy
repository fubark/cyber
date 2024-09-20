use t 'test'

--|
--| float
--|

func staticPrim(a float) bool:
	return a == 123.0

type PrimType:
	a float

-- Call static function with literal.
t.eq(staticPrim(123.0), true)

-- Call static function infer type.
t.eq(staticPrim(123), true)

-- Call static function with var.
var n = 123.0
t.eq(staticPrim(n), true)

-- Call static function from dyn.
dyn n_dyn = 123.0
t.eq(staticPrim(n_dyn), true)

-- Call static function with object access.
var o = PrimType{a=123.0}
t.eq(staticPrim(o.a), true)

--|
--| int
--|
func fooInt(a int) bool:
    return a == 123

-- Literal.
t.eq(fooInt(123), true)
        
-- From var.
var i = 123
t.eq(fooInt(i), true)

-- Cast erased type.
dyn i_dyn = 123
t.eq(fooInt(i), true)

--|
--| pointer.
--|
func fooPointer(a *void) bool:
    return a.addr() == 123

-- From var.
var ptr = pointer.fromAddr(void, 123)
t.eq(fooPointer(ptr), true)

-- Cast erased type.
dyn ptr_dyn = pointer.fromAddr(void, 123)
t.eq(fooPointer(ptr_dyn), true)

--|
--| String.
--|
func fooString(a String) bool:
    return a == 'true'

-- Literal.
t.eq(fooString('true'), true)

-- From var.
var str = 'true'
t.eq(fooString(str), true)

-- Cast erased type.
dyn str_dyn = 'true'
t.eq(fooString(String(str_dyn)), true)

-- bool.
func fooBool(a bool) bool:
    return a

-- Literal.
t.eq(fooBool(true), true)

-- From var.
var b = true
t.eq(fooBool(b), true)

-- Cast erased type.
dyn b_dyn = true
t.eq(fooBool(bool(b_dyn)), true)

--|
--| Map
--|
func fooMap(a Map) bool:
    return a['a'] == 123

-- Literal.
t.eq(fooMap(Map{a=123}), true)

-- From var.
var map = Map{a=123}
t.eq(fooMap(map), true)

-- Cast erased type.
dyn map_dyn = Map{a=123}
t.eq(fooMap(map_dyn), true)

--|
--| List
--|
func fooList(a List[dyn]) bool:
    return a[0] == 123

-- Literal.
t.eq(fooList(.{123}), true)

-- From var.
var list = List[dyn]{123}
t.eq(fooList(list), true)

-- Cast erased type.
dyn list_dyn = List[dyn]{123}
t.eq(fooList(list_dyn), true)

--|
--| symbol
--|
func fooSymbol(a symbol) bool:
    return a == .sometag

-- Literal.
t.eq(fooSymbol(.sometag), true)

-- From var.
var tag = symbol.sometag
t.eq(fooSymbol(tag), true)

-- Cast erased type.
dyn tag_dyn = symbol.sometag
t.eq(fooSymbol(tag_dyn), true)

--cytest: pass