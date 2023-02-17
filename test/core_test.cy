import t 'test'
import os 'os'

-- arrayFill with primitive.
a = arrayFill(123, 10)
try t.eq(a.len(), 10)
for 0..10 each i:
  try t.eq(a[i], 123)

-- arrayFill with object performs shallow copy.
a = arrayFill([], 2)
try t.eq(a.len(), 2)
try t.eq(a[0] == a[1], false)

-- asciiCode()
try t.eq(asciiCode('a'), 97)

-- copy()
try t.eq(copy(123), 123)
object S:
  foo
  bar
s = S{}
oldList = [123, s]
newList = copy(oldList)
try t.eq(newList == oldList, false)
try t.eq(newList.len(), 2)
try t.eq(newList[0], 123)
try t.eq(newList[1], s)
oldMap = { a: 123, b: s }
newMap = copy(oldMap)
try t.eq(newMap == oldMap, false)
try t.eq(newMap.size(), 2)
try t.eq(newMap.a, 123)
try t.eq(newMap.b, s)
oldStr = 'foo'
newStr = copy(oldStr)
try t.eq(newStr, oldStr)
rcList = []
s.foo = 123
s.bar = rcList
newS = copy(s)
try t.eq(newS == s, false)
try t.eq(newS.foo, 123)
try t.eq(newS.bar, rcList)

-- int()
res = int(100)
try t.eq(valtag(res), #int)
try t.eq(number(res), 100)
try t.eq(number(int(100.1)), 100)
try t.eq(number(int('100')), 100)
try t.eq(number(int('100.1')), 100)

-- number()
try t.eq(number(100), 100)
try t.eq(number(100.1), 100.1)
try t.eq(number('100'), 100)
try t.eq(number('100.1'), 100.1)

-- string()
str = 'abcd'
try t.eq(string(str), 'abcd')
try t.eq(string(str[0..2]), 'ab')
rstr = rawstring('abcd')
try t.eq(string(rstr), 'rawstring (4)')
try t.eq(string(rstr[0..2]), 'rawstring (2)')
try t.eq(string(123), '123')
try t.eq(string(123.4), '123.4')
try t.eq(string(123.456), '123.456')
try t.eq(string(123.00000123), '123.00000123')
try t.eq(string(int(123)), '123')
try t.eq(string(error(#foo)), 'error#foo')
try t.eq(string(#foo), '#foo')

-- typeid()
try t.eq(typeid(none), 0)
try t.eq(typeid(true), 1)
try t.eq(typeid(false), 1)
try t.eq(typeid(error(#err)), 2)
try t.eq(typeid('abc'), 3)
try t.eq(typeid('abc🦊'), 4)
try t.eq(typeid(#abc), 6)
try t.eq(typeid(int(123)), 7)
try t.eq(typeid(123), 8)
try t.eq(typeid([]), 9)
try t.eq(typeid({}), 11)

-- valtag()
try t.eq(valtag(123), #number)
try t.eq(valtag('abc'), #string)
try t.eq(valtag(opaque(0)), #pointer)

-- writeFile() rawstring
if os.system != 'wasm':
  s = rawstring('').insertByte(0, 255)
  writeFile('test.txt', s)
  read = readFile('test.txt')
  try t.eq(read.len(), 1)
  try t.eq(read.byteAt(0), 255)