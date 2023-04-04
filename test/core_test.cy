import t 'test'
import os 'os'

-- arrayFill with primitive.
a = arrayFill(123, 10)
t.eq(a.len(), 10)
for 0..10 each i:
  t.eq(a[i], 123)

-- arrayFill with object performs shallow copy.
a = arrayFill([], 2)
t.eq(a.len(), 2)
t.eq(a[0] == a[1], false)

-- copy()
t.eq(copy(123), 123)
type S object:
  foo
  bar
s = S{}
oldList = [123, s]
newList = copy(oldList)
t.eq(newList == oldList, false)
t.eq(newList.len(), 2)
t.eq(newList[0], 123)
t.eq(newList[1], s)
oldMap = { a: 123, b: s }
newMap = copy(oldMap)
t.eq(newMap == oldMap, false)
t.eq(newMap.size(), 2)
t.eq(newMap.a, 123)
t.eq(newMap.b, s)
oldStr = 'foo'
newStr = copy(oldStr)
t.eq(newStr, oldStr)
rcList = []
s.foo = 123
s.bar = rcList
newS = copy(s)
t.eq(newS == s, false)
t.eq(newS.foo, 123)
t.eq(newS.bar, rcList)

-- error()
-- See error_test.cy

-- errorReport(), current frame.
try:
  throw error.Boom
catch:
  t.eq(errorReport(), "main:49:3 main:
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
    t.eq(errorReport(), "main:58:3 foo2:
  throw error.Boom
  ^
main:61:5 foo:
    foo2()
    ^
main:73:1 main:
foo()
^
")
foo()

-- toInteger()
res = toInteger(100)
t.eq(typesym(res), #int)
t.eq(toNumber(res), 100)
t.eq(toNumber(toInteger(100.1)), 100)
t.eq(toNumber(toInteger('100')), 100)
t.eq(toNumber(toInteger('100.1')), 100)

-- toNumber()
t.eq(toNumber(100), 100)
t.eq(toNumber(100.1), 100.1)
t.eq(toNumber('100'), 100)
t.eq(toNumber('100.1'), 100.1)

-- parseCyon()
val = parseCyon('123')
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

-- toPointer()
ptr = toPointer(0xDEADBEEF)
t.eq(ptr.value(), 3735928559)

-- toString()
str = 'abcd'
t.eq(toString(str), 'abcd')
t.eq(toString(str[0..2]), 'ab')
rstr = toRawstring('abcd')
t.eq(toString(rstr), 'rawstring (4)')
t.eq(toString(rstr[0..2]), 'rawstring (2)')
t.eq(toString(123), '123')
t.eq(toString(123.4), '123.4')
t.eq(toString(123.456), '123.456')
t.eq(toString(123.00000123), '123.00000123')
t.eq(toString(toInteger(123)), '123')
t.eq(toString(error.foo), 'error.foo')
t.eq(toString(#foo), '#foo')
t.eq(toString(number), 'type: number')

-- toCyon()
cyon = toCyon(123)
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
t.eq(typeof(123), number)
t.eq(typeof(toPointer(123)), pointer)
t.eq(typeof('abc'), typeof('xyz'))
t.eq(typeof(toRawstring('abc')), typeof(toRawstring('xyz')))
t.eq(typeof(error.Foo), error)
t.eq(typeof([]), List)
t.eq(typeof({}), Map)

-- typesym()
t.eq(typesym(123), #number)
t.eq(typesym('abc'), #string)
t.eq(typesym(toPointer(0)), #pointer)

-- writeFile() rawstring
if os.system != 'wasm':
  s = toRawstring('').insertByte(0, 255)
  writeFile('test.txt', s)
  read = readFile('test.txt')
  t.eq(read.len(), 1)
  t.eq(read.byteAt(0), 255)