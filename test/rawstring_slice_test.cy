-- Copyright (c) 2023 Cyber (See LICENSE)

-- Same tests as rawstring_test.cy except using a slice.

import t 'test'

str = rawstring('abc🦊xyz🐶')
str = str[0..]  -- Sets up the slice.
try t.eq(str, rawstring('abc🦊xyz🐶'))

-- Sets up the slice
upper = rawstring('ABC🦊XYZ🐶')[0..]

-- index operator
try t.eq(str[-1], error.InvalidRune)
try t.eq(str[-4], '🐶')
try t.eq(str[0], 'a')
try t.eq(str[0].isAscii(), true)
try t.eq(str[3], '🦊')
try t.eq(str[3].isAscii(), false)
try t.eq(str[4], error.InvalidRune)
try t.eq(str[10], '🐶')
try t.eq(str[13], error.InvalidRune)
try t.eq(str[14], error.OutOfBounds)

-- slice operator
try t.eq(str[0..], rawstring('abc🦊xyz🐶'))
try t.eq(str[7..], rawstring('xyz🐶'))
try t.eq(str[10..], rawstring('🐶'))
try t.eq(str[-4..], rawstring('🐶'))
try t.eq(str[14..], rawstring(''))
try t.eq(str[15..], error.OutOfBounds)
try t.eq(str[-20..], error.OutOfBounds)
try t.eq(str[..0], rawstring(''))
try t.eq(str[..7], rawstring('abc🦊'))
try t.eq(str[..10], rawstring('abc🦊xyz'))
try t.eq(str[..-4], rawstring('abc🦊xyz'))
try t.eq(str[..14], rawstring('abc🦊xyz🐶'))
try t.eq(str[..15], error.OutOfBounds)
try t.eq(str[0..0], rawstring(''))
try t.eq(str[0..1], rawstring('a'))
try t.eq(str[7..14], rawstring('xyz🐶'))
try t.eq(str[10..14], rawstring('🐶'))
try t.eq(str[14..14], rawstring(''))
try t.eq(str[14..15], error.OutOfBounds)
try t.eq(str[3..1], error.OutOfBounds)

-- byteAt()
try t.eq(str.byteAt(-1), error.OutOfBounds)
try t.eq(str.byteAt(0), 97)
try t.eq(str.byteAt(3), 240)
try t.eq(str.byteAt(4), 159)
try t.eq(str.byteAt(10), 240)
try t.eq(str.byteAt(13), 182)
try t.eq(str.byteAt(14), error.OutOfBounds)

-- concat()
try t.eq(str.concat('123'), rawstring('abc🦊xyz🐶123'))

-- endsWith()
try t.eq(str.endsWith('xyz🐶'), true)
try t.eq(str.endsWith('xyz'), false)

-- find()
try t.eq(str.find('bc🦊'), 1)
try t.eq(str.find('xy'), 7)
try t.eq(str.find('bd'), none)
try t.eq(str.find('ab'), 0)

-- findAnyRune()
try t.eq(str.findAnyRune('a'), 0)
try t.eq(str.findAnyRune('🦊'), 3)
try t.eq(str.findAnyRune('🦊a'), 0)
try t.eq(str.findAnyRune('xy'), 7)
try t.eq(str.findAnyRune('ef'), none)

-- findRune()
try t.eq(str.findRune(0u'a'), 0)
try t.eq(str.findRune(0u'🦊'), 3)
try t.eq(str.findRune(0u'x'), 7)
try t.eq(str.findRune(0u'd'), none)
try t.eq(str.findRune(97), 0)
try t.eq(str.findRune(129418), 3)
try t.eq(str.findRune(128054), 10)
try t.eq(str.findRune(100), none)

-- insertByte()
try t.eq(str.insertByte(2, 97), rawstring('abac🦊xyz🐶'))

-- insert()
try t.eq(str.insert(-1, 'foo'), error.OutOfBounds)
try t.eq(str.insert(0, 'foo'), rawstring('fooabc🦊xyz🐶'))
try t.eq(str.insert(3, 'foo🦊'), rawstring('abcfoo🦊🦊xyz🐶'))
try t.eq(str.insert(10, 'foo'), rawstring('abc🦊xyzfoo🐶'))
try t.eq(str.insert(14, 'foo'), rawstring('abc🦊xyz🐶foo'))
try t.eq(str.insert(15, 'foo'), error.OutOfBounds)

-- isAscii()
try t.eq(str.isAscii(), false)
try t.eq(rawstring('abc').isAscii(), true)

-- len()
try t.eq(str.len(), 14)

-- less()
try t.eq(str.less('ac'), true)
try t.eq(str.less('aa'), false)

-- lower()
try t.eq(upper.lower(), rawstring('abc🦊xyz🐶'))

-- repeat()
try t.eq(str.repeat(-1), error.InvalidArgument)
try t.eq(str.repeat(0), rawstring(''))
try t.eq(str.repeat(1), rawstring('abc🦊xyz🐶'))
try t.eq(str.repeat(2), rawstring('abc🦊xyz🐶abc🦊xyz🐶'))

-- replace()
try t.eq(str.replace('abc🦊', 'foo'), rawstring('fooxyz🐶'))
try t.eq(str.replace('bc🦊', 'foo'), rawstring('afooxyz🐶'))
try t.eq(str.replace('bc', 'foo🦊'), rawstring('afoo🦊🦊xyz🐶'))
try t.eq(str.replace('xy', 'foo'), rawstring('abc🦊fooz🐶'))
try t.eq(str.replace('xyz🐶', 'foo'), rawstring('abc🦊foo'))
try t.eq(str.replace('abcd', 'foo'), rawstring('abc🦊xyz🐶'))

-- runeAt()
try t.eq(str.runeAt(-1), error.OutOfBounds)
try t.eq(str.runeAt(0), 97)
try t.eq(str.runeAt(3), 129418)
try t.eq(str.runeAt(4), error.InvalidRune)
try t.eq(str.runeAt(10), 128054)
try t.eq(str.runeAt(13), error.InvalidRune)
try t.eq(str.runeAt(14), error.OutOfBounds)

-- sliceAt().
try t.eq(str.sliceAt(-1), error.OutOfBounds)
try t.eq(str.sliceAt(0), 'a')
try t.eq(str.sliceAt(0).isAscii(), true)
try t.eq(str.sliceAt(3), '🦊')
try t.eq(str.sliceAt(3).isAscii(), false)
try t.eq(str.sliceAt(4), error.InvalidRune)
try t.eq(str.sliceAt(10), '🐶')
try t.eq(str.sliceAt(13), error.InvalidRune)
try t.eq(str.sliceAt(14), error.OutOfBounds)

-- split()
res = rawstring('abc,🐶ab,a')[0..].split(',')
try t.eq(res.len(), 3)
try t.eq(res[0], rawstring('abc'))
try t.eq(res[1], rawstring('🐶ab'))
try t.eq(res[2], rawstring('a'))

-- startsWith()
try t.eq(str.startsWith('abc🦊'), true)
try t.eq(str.startsWith('bc🦊'), false)

-- trim()
try t.eq(str.trim(#left, 'a'), rawstring('bc🦊xyz🐶'))
try t.eq(str.trim(#right, '🐶'), rawstring('abc🦊xyz'))
try t.eq(str.trim(#ends, 'a🐶'), rawstring('bc🦊xyz'))

-- upper()
try t.eq(str.upper(), rawstring('ABC🦊XYZ🐶'))

-- utf8()
try t.eq(str.utf8(), 'abc🦊xyz🐶')
try t.eq(str.utf8().isAscii(), false)
try t.eq(rawstring('abc').utf8(), 'abc')
try t.eq(rawstring('abc').isAscii(), true)
try t.eq(rawstring('').insertByte(0, 255).utf8(), error.InvalidRune)