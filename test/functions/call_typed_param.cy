use t 'test'

--|
--| float
--|

fn staticPrim(a float) -> bool:
	return a == 123.0

type PrimType:
	a float

-- Call static function with literal.
t.eq(staticPrim(123.0), true)

-- Call static function infer type.
t.eq(staticPrim(123), true)

-- Call static function with var.
n := 123.0
t.eq(staticPrim(n), true)

-- Call static function with object access.
o := PrimType{a=123.0}
t.eq(staticPrim(o.a), true)

--|
--| int
--|
fn fooInt(a int) -> bool:
    return a == 123

-- Literal.
t.eq(fooInt(123), true)
        
-- From var.
i := 123
t.eq(fooInt(i), true)

--|
--| Ptr.
--|
fn fooPointer(a Ptr[void]) -> bool:
    return as[int] a == 123

-- From var.
ptr := as[Ptr[void]] 123
t.eq(fooPointer(ptr), true)

--|
--| str.
--|
fn fooString(a str) -> bool:
    return a == 'true'

-- Literal.
t.eq(fooString('true'), true)

-- From var.
s := 'true'
t.eq(fooString(s), true)

-- bool.
fn fooBool(a bool) -> bool:
    return a

-- Literal.
t.eq(fooBool(true), true)

-- From var.
b := true
t.eq(fooBool(b), true)

--|
--| Map
--|
fn fooMap(a Map[str, int]) -> bool:
    return a['a'] == 123

-- Literal.
t.eq(fooMap({a=123}), true)

-- From var.
m := Map[str, int]{a=123}
t.eq(fooMap(move m), true)

--|
--| Slice
--|
fn foo_slice(a []int) -> bool:
    return a[0] == 123

-- Literal.
t.eq(true, foo_slice({123}))

-- From var.
arr := []int{123}
t.eq(true, foo_slice(arr))

--|
--| symbol
--|
fn fooSymbol(a symbol) -> bool:
    return a == @sometag

-- Literal.
t.eq(fooSymbol(@sometag), true)

-- From var.
tag := @sometag
t.eq(fooSymbol(tag), true)

--cytest: pass