-- Copyright (c) 2023 Cyber (See LICENSE)

-- Same tests as rawstring_test.cy except using a slice.

import t 'test'

str = rawstring('abc🦊xyz🐶')
str = str[0..]  -- Sets up the slice.
t.eq(str, rawstring('abc🦊xyz🐶'))

-- Sets up the slice
upper = rawstring('ABC🦊XYZ🐶')[0..]

-- index operator
t.eq(try str[-1], error.InvalidRune)
t.eq(str[-4], '🐶')
t.eq(str[0], 'a')
t.eq(str[0].isAscii(), true)
t.eq(str[3], '🦊')
t.eq(str[3].isAscii(), false)
t.eq(try str[4], error.InvalidRune)
t.eq(str[10], '🐶')
t.eq(try str[13], error.InvalidRune)
t.eq(try str[14], error.OutOfBounds)

-- slice operator
t.eq(str[0..], rawstring('abc🦊xyz🐶'))
t.eq(str[7..], rawstring('xyz🐶'))
t.eq(str[10..], rawstring('🐶'))
t.eq(str[-4..], rawstring('🐶'))
t.eq(str[14..], rawstring(''))
t.eq(try str[15..], error.OutOfBounds)
t.eq(try str[-20..], error.OutOfBounds)
t.eq(str[..0], rawstring(''))
t.eq(str[..7], rawstring('abc🦊'))
t.eq(str[..10], rawstring('abc🦊xyz'))
t.eq(str[..-4], rawstring('abc🦊xyz'))
t.eq(str[..14], rawstring('abc🦊xyz🐶'))
t.eq(try str[..15], error.OutOfBounds)
t.eq(str[0..0], rawstring(''))
t.eq(str[0..1], rawstring('a'))
t.eq(str[7..14], rawstring('xyz🐶'))
t.eq(str[10..14], rawstring('🐶'))
t.eq(str[14..14], rawstring(''))
t.eq(try str[14..15], error.OutOfBounds)
t.eq(try str[3..1], error.OutOfBounds)

-- byteAt()
t.eq(try str.byteAt(-1), error.OutOfBounds)
t.eq(str.byteAt(0), 97)
t.eq(str.byteAt(3), 240)
t.eq(str.byteAt(4), 159)
t.eq(str.byteAt(10), 240)
t.eq(str.byteAt(13), 182)
t.eq(try str.byteAt(14), error.OutOfBounds)

-- concat()
t.eq(str.concat('123'), rawstring('abc🦊xyz🐶123'))

-- endsWith()
t.eq(str.endsWith('xyz🐶'), true)
t.eq(str.endsWith('xyz'), false)

-- find()
t.eq(str.find('bc🦊'), 1)
t.eq(str.find('xy'), 7)
t.eq(str.find('bd'), none)
t.eq(str.find('ab'), 0)

-- findAnyRune()
t.eq(str.findAnyRune('a'), 0)
t.eq(str.findAnyRune('🦊'), 3)
t.eq(str.findAnyRune('🦊a'), 0)
t.eq(str.findAnyRune('xy'), 7)
t.eq(str.findAnyRune('ef'), none)

-- findRune()
t.eq(str.findRune(0u'a'), 0)
t.eq(str.findRune(0u'🦊'), 3)
t.eq(str.findRune(0u'x'), 7)
t.eq(str.findRune(0u'd'), none)
t.eq(str.findRune(97), 0)
t.eq(str.findRune(129418), 3)
t.eq(str.findRune(128054), 10)
t.eq(str.findRune(100), none)

-- insertByte()
t.eq(str.insertByte(2, 97), rawstring('abac🦊xyz🐶'))

-- insert()
t.eq(try str.insert(-1, 'foo'), error.OutOfBounds)
t.eq(str.insert(0, 'foo'), rawstring('fooabc🦊xyz🐶'))
t.eq(str.insert(3, 'foo🦊'), rawstring('abcfoo🦊🦊xyz🐶'))
t.eq(str.insert(10, 'foo'), rawstring('abc🦊xyzfoo🐶'))
t.eq(str.insert(14, 'foo'), rawstring('abc🦊xyz🐶foo'))
t.eq(try str.insert(15, 'foo'), error.OutOfBounds)

-- isAscii()
t.eq(str.isAscii(), false)
t.eq(rawstring('abc').isAscii(), true)

-- len()
t.eq(str.len(), 14)

-- less()
t.eq(str.less('ac'), true)
t.eq(str.less('aa'), false)

-- lower()
t.eq(upper.lower(), rawstring('abc🦊xyz🐶'))

-- repeat()
t.eq(try str.repeat(-1), error.InvalidArgument)
t.eq(str.repeat(0), rawstring(''))
t.eq(str.repeat(1), rawstring('abc🦊xyz🐶'))
t.eq(str.repeat(2), rawstring('abc🦊xyz🐶abc🦊xyz🐶'))

-- replace()
t.eq(str.replace('abc🦊', 'foo'), rawstring('fooxyz🐶'))
t.eq(str.replace('bc🦊', 'foo'), rawstring('afooxyz🐶'))
t.eq(str.replace('bc', 'foo🦊'), rawstring('afoo🦊🦊xyz🐶'))
t.eq(str.replace('xy', 'foo'), rawstring('abc🦊fooz🐶'))
t.eq(str.replace('xyz🐶', 'foo'), rawstring('abc🦊foo'))
t.eq(str.replace('abcd', 'foo'), rawstring('abc🦊xyz🐶'))

-- runeAt()
t.eq(try str.runeAt(-1), error.OutOfBounds)
t.eq(str.runeAt(0), 97)
t.eq(str.runeAt(3), 129418)
t.eq(try str.runeAt(4), error.InvalidRune)
t.eq(str.runeAt(10), 128054)
t.eq(try str.runeAt(13), error.InvalidRune)
t.eq(try str.runeAt(14), error.OutOfBounds)

-- sliceAt().
t.eq(try str.sliceAt(-1), error.OutOfBounds)
t.eq(str.sliceAt(0), 'a')
t.eq(str.sliceAt(0).isAscii(), true)
t.eq(str.sliceAt(3), '🦊')
t.eq(str.sliceAt(3).isAscii(), false)
t.eq(try str.sliceAt(4), error.InvalidRune)
t.eq(str.sliceAt(10), '🐶')
t.eq(try str.sliceAt(13), error.InvalidRune)
t.eq(try str.sliceAt(14), error.OutOfBounds)

-- split()
res = rawstring('abc,🐶ab,a')[0..].split(',')
t.eq(res.len(), 3)
t.eq(res[0], rawstring('abc'))
t.eq(res[1], rawstring('🐶ab'))
t.eq(res[2], rawstring('a'))

-- startsWith()
t.eq(str.startsWith('abc🦊'), true)
t.eq(str.startsWith('bc🦊'), false)

-- trim()
t.eq(str.trim(#left, 'a'), rawstring('bc🦊xyz🐶'))
t.eq(str.trim(#right, '🐶'), rawstring('abc🦊xyz'))
t.eq(str.trim(#ends, 'a🐶'), rawstring('bc🦊xyz'))

-- upper()
t.eq(str.upper(), rawstring('ABC🦊XYZ🐶'))

-- utf8()
t.eq(str.utf8(), 'abc🦊xyz🐶')
t.eq(str.utf8().isAscii(), false)
t.eq(rawstring('abc').utf8(), 'abc')
t.eq(rawstring('abc').isAscii(), true)
t.eq(try rawstring('').insertByte(0, 255).utf8(), error.InvalidRune)