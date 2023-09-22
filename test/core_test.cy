import t 'test'
import os 'os'

--| Tests sensitive to line numbers.
-- errorReport(), current frame.
try:
  throw error.Boom
catch:
  t.eq(errorReport(), "main:7:3 main:
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
    t.eq(errorReport(), "main:16:3 foo2:
  throw error.Boom
  ^
main:19:5 foo:
    foo2()
    ^
main:31:1 main:
foo()
^
")
foo()

-- arrayFill with primitive.
var a = arrayFill(123, 10)
t.eq(a.len(), 10)
for 0..10 each i:
  t.eq(a[i], 123)

-- arrayFill with object performs shallow copy.
a = arrayFill([], 2)
t.eq(a.len(), 2)
t.eq(a[0] == a[1], false)

-- boolean(), see truthy_test.cy

-- copy()
t.eq(copy(123), 123)
type S object:
  foo
  bar
var s = S{}
var oldList = [123, s]
var newList = copy(oldList)
t.eq(newList == oldList, false)
t.eq(newList.len(), 2)
t.eq(newList[0], 123)
t.eq(newList[1], s)
var oldMap = { a: 123, b: s }
var newMap = copy(oldMap)
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
var newS = copy(s)
t.eq(newS == s, false)
t.eq(newS.foo, 123)
t.eq(newS.bar, rcList)

-- error(), see error_test.cy

-- float(), see float_test.cy

-- int()
var res = int('100')
t.eq(typesym(res), #int)
t.eq(res, 100)
t.eq(int(100.1), 100)
t.eq(int('100'), 100)
t.eq(int('100.1'), 100)

-- isAlpha()
t.eq(isAlpha(0u'3'), false)
t.eq(isAlpha(0u'a'), true)
t.eq(isAlpha(0u'A'), true)

-- isDigit()
t.eq(isDigit(0u'3'), true)
t.eq(isDigit(0u'a'), false)
t.eq(isDigit(0u'A'), false)

-- parseCyber()
res = parseCyber('var foo: 123')
t.eq(res['decls'][0].type, 'variable')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('type foo bar')
t.eq(res['decls'][0].type, 'typeAlias')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('func foo(): pass')
t.eq(res['decls'][0].type, 'func')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('func foo() = bar')
t.eq(res['decls'][0].type, 'funcInit')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('import foo \'bar\'')
t.eq(res['decls'][0].type, 'import')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('type foo object:\n  a')
t.eq(res['decls'][0].type, 'object')
t.eq(res['decls'][0].name, 'foo')

res = parseCyber('type foo enum:\n  a')
t.eq(res['decls'][0].type, 'enumT')
t.eq(res['decls'][0].name, 'foo')

-- parseCyon()
var val = parseCyon('123')
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
val = parseCyon('\{\}')
t.eq(val.size(), 0)
val = parseCyon('\{ a: 123 \}')
t.eq(val.size(), 1)
t.eq(val['a'], 123)

-- pointer()
var ptr = pointer(0xDEADBEEF)
t.eq(ptr.value(), 3735928559)

-- runestr()
t.eq(runestr(0u'a'), 'a')
t.eq(runestr(0u'🦊'), '🦊')
t.eq(try runestr(2 ^ 22), error.InvalidRune)
t.eq(try runestr(-1), error.InvalidRune)

-- string()
var str = 'abcd'
t.eq(string(str), 'abcd')
t.eq(string(str[0..2]), 'ab')
var rstr = rawstring('abcd')
t.eq(string(rstr), 'rawstring (4)')
t.eq(string(rstr[0..2]), 'rawstring (2)')
t.eq(string(123), '123')
t.eq(string(123.4), '123.4')
t.eq(string(123.456), '123.456')
t.eq(string(123.00000123), '123.00000123')
t.eq(string(int(123)), '123')
t.eq(string(error.foo), 'error.foo')
t.eq(string(#foo), '#foo')
t.eq(string(float), 'type: float')

-- toCyon()
var cyon = toCyon(123)
t.eq(cyon, '123')
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
    1
    2
    3
]")
cyon = toCyon({})
t.eq(cyon, "\{\}")
cyon = toCyon({ a: 123 })
t.eq(cyon, "\{
    a: 123
\}")

-- typeof()
t.eq(typeof(true), boolean)
t.eq(typeof(123), int)
t.eq(typeof(123.0), float)
t.eq(typeof(pointer(123)), pointer)
t.eq(typeof('abc'), typeof('xyz'))
t.eq(typeof(rawstring('abc')), typeof(rawstring('xyz')))
t.eq(typeof(error.Foo), error)
t.eq(typeof([]), List)
t.eq(typeof({}), Map)

-- typesym()
t.eq(typesym(123), #int)
t.eq(typesym(123.0), #float)
t.eq(typesym('abc'), #string)
t.eq(typesym(pointer(0)), #pointer)