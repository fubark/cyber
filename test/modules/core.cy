use t 'test'

--| Tests sensitive to line numbers.
-- errorReport(), current frame.
try:
    throw error.Boom
catch:
    t.eq(errorReport(), '''main:6:5 main:
    throw error.Boom
    ^
''')

-- errorReport(), one frame before and one frame after.
func foo2():
    throw error.Boom
func foo():
    try:
        foo2()
    catch:
        t.eq(errorReport(), '''main:15:5 foo2:
    throw error.Boom
    ^
main:18:9 foo:
        foo2()
        ^
main:30:1 main:
foo()
^
''')
foo()

-- bool(), see truthy_test.cy

-- choicetag(), see choice_type.cy

-- copy()
t.eq(copy(123), 123)
type S:
    foo any
    bar any
var s = S{}
-- var oldList = List[dyn]{123, s}
-- var newList = copy(oldList)
-- t.eq(newList == oldList, false)
-- t.eq(newList.len(), 2)
-- t.eq(newList[0], 123)
-- t.eq(newList[1].foo, s.foo)
-- t.eq(newList[1].bar, s.bar)
var oldMap = Map{a=123, b=s}
var newMap = copy(oldMap)
t.eq(newMap == oldMap, false)
t.eq(newMap.size(), 2)
t.eq(newMap['a'], 123)
t.eq(newMap['b'].foo, s.foo)
t.eq(newMap['b'].bar, s.bar)
var oldStr = 'foo'
var newStr = copy(oldStr)
t.eq(newStr, oldStr)
var rcList = List[dyn]{}
s.foo = 123
s.bar = rcList
dyn newS = copy(s)
t.eq(newS == s, false)
t.eq(newS.foo, 123)
t.eq(newS.bar, rcList)

-- error(), see error_test.cy

-- float(), see float_test.cy

-- int()
dyn res = int('100')
t.eq(typeOf(res), dyn)
t.eq(res, 100)
t.eq(int(100.1), 100)
t.eq(int('100'), 100)
t.eq(int('100.1'), 100)

-- is()
t.eq(is(1, 2), false)
t.eq(is(2, 2), false)
var a any = 2
t.eq(is(a, a), true)
t.eq(is(List[dyn]{}, List[dyn]{}), false)
var list = List[dyn]{}
var list2 = list
t.eq(is(list, list2), true)

-- isAlpha()
t.eq(isAlpha(`3`), false)
t.eq(isAlpha(`a`), true)
t.eq(isAlpha(`A`), true)

-- isDigit()
t.eq(isDigit(`3`), true)
t.eq(isDigit(`a`), false)
t.eq(isDigit(`A`), false)

-- isNone()
var opt ?int = none
t.eq(isNone(opt), true)
t.eq(isNone(1), false)
opt = 123
t.eq(isNone(opt), false)

-- runestr()
t.eq(runestr(`a`), 'a')
t.eq(runestr(`🦊`), '🦊')
t.eq(try runestr(2 ^ 22), error.InvalidRune)
t.eq(try runestr(-1), error.InvalidRune)

-- string()
var str = 'abcd'
t.eq(string(str), 'abcd')
t.eq(string(str[0..2]), 'ab')
t.eq(string(123), '123')
t.eq(string(123.4), '123.4')
t.eq(string(123.456), '123.456')
t.eq(string(123.00000123), '123.00000123')
t.eq(string(int(123)), '123')
t.eq(string(error.foo), 'error.foo')
t.eq(string(symbol.foo), 'symbol.foo')
t.eq(string(float), 'type: float')

-- typeOf()
t.eq(typeOf(true), bool)
t.eq(typeOf(false), bool)
t.eq(typeOf(123), int)
t.eq(typeOf(123.0), float)
t.eq(typeOf(pointer.fromAddr(void, 123)), *void)
t.eq(typeOf('abc'), string)
t.eq(typeOf('abc🦊'), string)
t.eq(typeOf(symbol.abc), symbol)
t.eq(typeOf(error.Foo), error)
t.eq(typeOf(List[dyn]{}), List[dyn])
t.eq(typeOf(Map{}), Map)
t.eq(typeOf({}), ^Table)
type Foo:
    a float
var foo = Foo{a=123}
t.eq(typeOf(foo), Foo)

-- typeInfo(), see type.cy

--cytest: pass