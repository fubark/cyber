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

-- string()
str = 'abcd'
try t.eq(string(str[0..2]), 'ab')
rstr = rawstring('abcd')
try t.eq(string(rstr[0..2]), 'ab')

-- writeFile() rawstring
if os.system != 'wasm':
  s = rawstring('').insertByte(0, 255)
  writeFile('test.txt', s)
  read = readFile('test.txt')
  try t.eq(read.len(), 1)
  try t.eq(read.byteAt(0), 255)