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

-- copy()
t.eq(copy(123), 123)
type S:
    foo any
    bar any
var s = S{}
var oldList = {123, s}
var newList = copy(oldList) as List[dyn]
t.eq(newList == oldList, false)
t.eq(newList.len(), 2)
t.eq(newList[0], 123)
t.eq(newList[1], s)
var oldMap = Map{a=123, b=s}
let newMap = copy(oldMap)
t.eq(newMap == oldMap, false)
t.eq(newMap.size(), 2)
t.eq(newMap['a'], 123)
t.eq(newMap['b'], s)
var oldStr = 'foo'
var newStr = copy(oldStr)
t.eq(newStr, oldStr)
var rcList = {_}
s.foo = 123
s.bar = rcList
let newS = copy(s)
t.eq(newS == s, false)
t.eq(newS.foo, 123)
t.eq(newS.bar, rcList)

-- error(), see error_test.cy

-- float(), see float_test.cy

-- int()
let res = int('100')
t.eq(typeof(res), int)
t.eq(res, 100)
t.eq(int(100.1), 100)
t.eq(int('100'), 100)
t.eq(int('100.1'), 100)

-- is()
t.eq(is(1, 2), false)
t.eq(is(2, 2), false)
var a any = 2
t.eq(is(a, a), true)
t.eq(is({_}, {_}), false)
var list = {_}
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

-- String()
var str = 'abcd'
t.eq(String(str), 'abcd')
t.eq(String(str[0..2]), 'ab')
var rstr = Array('abcd')
t.eq(String(rstr), 'Array (4)')
t.eq(String(rstr[0..2]), 'Array (2)')
t.eq(String(123), '123')
t.eq(String(123.4), '123.4')
t.eq(String(123.456), '123.456')
t.eq(String(123.00000123), '123.00000123')
t.eq(String(int(123)), '123')
t.eq(String(error.foo), 'error.foo')
t.eq(String(symbol.foo), 'symbol.foo')
t.eq(String(float), 'metatype: float')

-- typeof()
t.eq(typeof(true), bool)
t.eq(typeof(123), int)
t.eq(typeof(123.0), float)
t.eq(typeof(pointer(void, 123)), metatype(*void))
t.eq(typeof('abc'), String)
t.eq(typeof(Array('abc')), typeof(Array('xyz')))
t.eq(typeof(error.Foo), error)
t.eq(typeof({_}), List[dyn])
t.eq(typeof(Map{}), Map)
t.eq(typeof({}), Table)

--cytest: pass