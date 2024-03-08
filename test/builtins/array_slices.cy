-- Same tests as array.cy except using a slice.

import t 'test'

var arr = Array('abc🦊xyz🐶')
arr = arr[0..]  -- Sets up the slice.
t.eq(arr, Array('abc🦊xyz🐶'))

-- Sets up the slice
var upper = Array('ABC🦊XYZ🐶')[0..]

-- index operator
t.eq(try arr[-1], error.OutOfBounds)
-- t.eq(arr[1-], 182)
-- t.eq(arr[4-], 240)
t.eq(arr[0], 97)
t.eq(arr[3], 240)
t.eq(try arr[14], error.OutOfBounds)

-- slice operator
t.eq(arr[0..], Array('abc🦊xyz🐶'))
t.eq(arr[7..], Array('xyz🐶'))
t.eq(arr[10..], Array('🐶'))
t.eq(try arr[-1..], error.OutOfBounds)
t.eq(try arr[..-1], error.OutOfBounds)
t.eq(try arr[15..], error.OutOfBounds)
t.eq(try arr[..15], error.OutOfBounds)
t.eq(try arr[14..15], error.OutOfBounds)
t.eq(try arr[3..1], error.OutOfBounds)
t.eq(arr[14..], Array(''))
t.eq(arr[..0], Array(''))
t.eq(arr[..7], Array('abc🦊'))
t.eq(arr[..10], Array('abc🦊xyz'))
t.eq(arr[..14], Array('abc🦊xyz🐶'))
t.eq(arr[0..0], Array(''))
t.eq(arr[0..1], Array('a'))
t.eq(arr[7..14], Array('xyz🐶'))
t.eq(arr[10..14], Array('🐶'))
t.eq(arr[14..14], Array(''))

-- concat()
t.eq(arr.concat(Array('123')), Array('abc🦊xyz🐶123'))

-- decode()
t.eq(arr.decode(), 'abc🦊xyz🐶')
t.eq(arr.decode().isAscii(), false)
t.eq(Array('abc').decode(), 'abc')
t.eq(Array('abc').decode().isAscii(), true)
t.eq(try Array('').insertByte(0, 255).decode(), error.Unicode)

-- endsWith()
t.eq(arr.endsWith(Array('xyz🐶')), true)
t.eq(arr.endsWith(Array('xyz')), false)

-- find()
t.eq(arr.find(Array('bc🦊')).?, 1)
t.eq(arr.find(Array('xy')).?, 7)
t.assert(arr.find(Array('bd')) == none)
t.eq(arr.find(Array('ab')).?, 0)

-- findAnyByte()
t.eq(arr.findAnyByte(Array('a')).?, 0)
t.eq(arr.findAnyByte(Array('xy')).?, 7)
t.assert(arr.findAnyByte(Array('ef')) == none)

-- findByte()
t.eq(arr.findByte(`a`).?, 0)
t.eq(arr.findByte(`x`).?, 7)
t.assert(arr.findByte(`d`) == none)
t.eq(arr.findByte(97).?, 0)
t.assert(arr.findByte(100) == none)

-- fmt()
t.eq(arr.fmt(.b), '0110000101100010011000111111000010011111101001101000101001111000011110010111101011110000100111111001000010110110')
t.eq(arr.fmt(.o), '141142143360237246212170171172360237220266')
t.eq(arr.fmt(.d), '097098099240159166138120121122240159144182')
t.eq(arr.fmt(.x), '616263f09fa68a78797af09f90b6')

-- getByte()
t.eq(arr.getByte(0), 97)
t.eq(arr.getByte(3), 240)
t.eq(arr.getByte(4), 159)
t.eq(arr.getByte(10), 240)
t.eq(arr.getByte(13), 182)
t.eq(try arr.getByte(-1), error.OutOfBounds)
t.eq(try arr.getByte(14), error.OutOfBounds)

-- getInt()
var iarr = Array('')
iarr = iarr.insertByte(0, 0x5a)
iarr = iarr.insertByte(1, 0xf1)
iarr = iarr.insertByte(2, 0x06)
iarr = iarr.insertByte(3, 0x04)
iarr = iarr.insertByte(4, 0x5e)
iarr = iarr.insertByte(5, 0xd2)
t.eq(iarr[0..].getInt(0, .big), 99991234567890)
t.eq(iarr.getInt(0, .little), -50173740388006)

-- getInt32()
iarr = Array('')
iarr = iarr.insertByte(0, 0x49)
iarr = iarr.insertByte(1, 0x96)
iarr = iarr.insertByte(2, 0x02)
iarr = iarr.insertByte(3, 0xD2)
t.eq(iarr[0..].getInt32(0, .big), 1234567890)
t.eq(iarr[0..].getInt32(0, .little), 3523384905)

-- insertByte()
t.eq(arr.insertByte(2, 97), Array('abac🦊xyz🐶'))

-- insert()
t.eq(try arr.insert(-1, Array('foo')), error.OutOfBounds)
t.eq(arr.insert(0, Array('foo')), Array('fooabc🦊xyz🐶'))
t.eq(arr.insert(3, Array('foo🦊')), Array('abcfoo🦊🦊xyz🐶'))
t.eq(arr.insert(10, Array('foo')), Array('abc🦊xyzfoo🐶'))
t.eq(arr.insert(14, Array('foo')), Array('abc🦊xyz🐶foo'))
t.eq(try arr.insert(15, Array('foo')), error.OutOfBounds)

-- len()
t.eq(arr.len(), 14)

-- repeat()
t.eq(try arr.repeat(-1), error.InvalidArgument)
t.eq(arr.repeat(0), Array(''))
t.eq(arr.repeat(1), Array('abc🦊xyz🐶'))
t.eq(arr.repeat(2), Array('abc🦊xyz🐶abc🦊xyz🐶'))

-- replace()
t.eq(arr.replace(Array('abc🦊'), Array('foo')), Array('fooxyz🐶'))
t.eq(arr.replace(Array('bc🦊'), Array('foo')), Array('afooxyz🐶'))
t.eq(arr.replace(Array('bc'), Array('foo🦊')), Array('afoo🦊🦊xyz🐶'))
t.eq(arr.replace(Array('xy'), Array('foo')), Array('abc🦊fooz🐶'))
t.eq(arr.replace(Array('xyz🐶'), Array('foo')), Array('abc🦊foo'))
t.eq(arr.replace(Array('abcd'), Array('foo')), Array('abc🦊xyz🐶'))

-- split()
var res = Array('abc,🐶ab,a')[0..].split(Array(','))
t.eq(res.len(), 3)
t.eq(res[0], Array('abc'))
t.eq(res[1], Array('🐶ab'))
t.eq(res[2], Array('a'))

-- trim()
t.eq(arr.trim(.left, Array('a')), Array('bc🦊xyz🐶'))
t.eq(arr.trim(.right, Array('🐶')), Array('abc🦊xyz'))
t.eq(arr.trim(.ends, Array('a🐶')), Array('bc🦊xyz'))

-- startsWith()
t.eq(arr.startsWith(Array('abc🦊')), true)
t.eq(arr.startsWith(Array('bc🦊')), false)

--cytest: pass