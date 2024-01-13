import t 'test'

--| Tests sensitive to line numbers.
-- errorReport(), current frame.
try:
    throw error.Boom
catch:
    t.eq(errorReport(), "main:6:5 main:
    throw error.Boom
    ^
")

-- errorReport(), one frame before and one frame after.
func foo2():
    throw error.Boom
func foo():
    try:
        foo2()
    catch:
        t.eq(errorReport(), "main:15:5 foo2:
    throw error.Boom
    ^
main:18:9 foo:
        foo2()
        ^
main:30:1 main:
foo()
^
")
foo()

-- bool(), see truthy_test.cy

-- copy()
t.eq(copy(123), 123)
type S:
    var foo
    var bar
var s = [S:]
var oldList = [123, s]
my newList = copy(oldList)
t.eq(newList == oldList, false)
t.eq(newList.len(), 2)
t.eq(newList[0], 123)
t.eq(newList[1], s)
var oldMap = [ a: 123, b: s ]
my newMap = copy(oldMap)
t.eq(newMap == oldMap, false)
t.eq(newMap.size(), 2)
t.eq(newMap.a, 123)
t.eq(newMap.b, s)
var oldStr = 'foo'
var newStr = copy(oldStr)
t.eq(newStr, oldStr)
var rcList = []
s.foo = 123
s.bar = rcList
my newS = copy(s)
t.eq(newS == s, false)
t.eq(newS.foo, 123)
t.eq(newS.bar, rcList)

-- error(), see error_test.cy

-- float(), see float_test.cy

-- int()
my res = int('100')
t.eq(typesym(res), .int)
t.eq(res, 100)
t.eq(int(100.1), 100)
t.eq(int('100'), 100)
t.eq(int('100.1'), 100)

-- is()
t.eq(is(1, 2), false)
t.eq(is(2, 2), true)
t.eq(is([], []), false)
var list = []
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

-- parseCyber()
res = parseCyber('var .foo = 123')
t.eq(res['decls'][0].type, 'variable')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('type foo bar')
t.eq(res['decls'][0].type, 'typeAlias')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('func foo(): pass')
t.eq(res['decls'][0].type, 'func')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('func Foo.foo(): pass')
t.eq(res['decls'][0].type, 'func')
t.eq(res['decls'][0].name, 'Foo.foo')

res = parseCyber('import foo \'bar\'')
t.eq(res['decls'][0].type, 'import')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('type foo:\n  var a')
t.eq(res['decls'][0].type, 'object')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('type foo enum:\n  case a')
t.eq(res['decls'][0].type, 'enumT')
t.eq(res['decls'][0].name, 'foo')

-- parseCyon()
my val = parseCyon('123')
t.eq(val, 123)
val = parseCyon('"foo"')
t.eq(val, 'foo')
val = parseCyon('true')
t.eq(val, true)
val = parseCyon('false')
t.eq(val, false)
val = parseCyon('[]')
t.eqList(val, [])
val = parseCyon('[1, 2, 3]')
t.eqList(val, [1, 2, 3])
val = parseCyon('[:]')
t.eq(val.size(), 0)
val = parseCyon('[ a: 123 ]')
t.eq(val.size(), 1)
t.eq(val['a'], 123)

-- pointer()
var ptr = pointer(0xDEADBEEF)
t.eq(ptr.addr(), 3735928559)

-- runestr()
t.eq(runestr(`a`), 'a')
t.eq(runestr(`🦊`), '🦊')
t.eq(try runestr(2 ^ 22), error.InvalidRune)
t.eq(try runestr(-1), error.InvalidRune)

-- string()
var str = 'abcd'
t.eq(string(str), 'abcd')
t.eq(string(str[0..2]), 'ab')
var rstr = array('abcd')
t.eq(string(rstr), 'array (4)')
t.eq(string(rstr[0..2]), 'array (2)')
t.eq(string(123), '123')
t.eq(string(123.4), '123.4')
t.eq(string(123.456), '123.456')
t.eq(string(123.00000123), '123.00000123')
t.eq(string(int(123)), '123')
t.eq(string(error.foo), 'error.foo')
t.eq(string(.foo), '.foo')
t.eq(string(float), 'type: float')

-- toCyon()
var cyon = toCyon(123)
t.eq(cyon, '123')
cyon = toCyon(123.0)
t.eq(cyon, '123.0')
cyon = toCyon('foo')
t.eq(cyon, "'foo'")
cyon = toCyon(true)
t.eq(cyon, 'true')
cyon = toCyon(false)
t.eq(cyon, 'false')
cyon = toCyon([])
t.eq(cyon, '[]')
cyon = toCyon([1, 2, 3])
t.eq(cyon, "[
    1,
    2,
    3,
]")
cyon = toCyon([:])
t.eq(cyon, "[:]")
cyon = toCyon([ a: 123 ])
t.eq(cyon, "[
    a: 123,
]")

-- typeof()
t.eq(typeof(true), bool)
t.eq(typeof(123), int)
t.eq(typeof(123.0), float)
t.eq(typeof(pointer(123)), pointer)
t.eq(typeof('abc'), typeof('xyz'))
t.eq(typeof(array('abc')), typeof(array('xyz')))
t.eq(typeof(error.Foo), error)
t.eq(typeof([]), List)
t.eq(typeof([:]), Map)

-- typesym()
t.eq(typesym(123), .int)
t.eq(typesym(123.0), .float)
t.eq(typesym('abc'), .string)
t.eq(typesym(pointer(0)), .pointer)

--cytest: pass