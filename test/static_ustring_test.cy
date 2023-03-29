-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Single quote literal.
str = 'abc🦊xyz🐶'
t.eq(str, 'abc🦊xyz🐶')

-- index operator
t.eq(str[-1], '🐶')
t.eq(str[0], 'a')
t.eq(str[0].isAscii(), true)
t.eq(str[3], '🦊')
t.eq(str[3].isAscii(), false)
t.eq(str[7], '🐶')
t.eq(try str[8], error.OutOfBounds)

-- slice operator
t.eq(str[0..], 'abc🦊xyz🐶')
t.eq(str[4..], 'xyz🐶')
t.eq(str[7..], '🐶')
t.eq(str[-1..], '🐶')
t.eq(str[8..], '')
t.eq(try str[9..], error.OutOfBounds)
t.eq(try str[-10..], error.OutOfBounds)
t.eq(str[..0], '')
t.eq(str[..4], 'abc🦊')
t.eq(str[..7], 'abc🦊xyz')
t.eq(str[..-1], 'abc🦊xyz')
t.eq(str[..8], 'abc🦊xyz🐶')
t.eq(try str[..9], error.OutOfBounds)
t.eq(str[0..0], '')
t.eq(str[0..1], 'a')
t.eq(str[4..8], 'xyz🐶')
t.eq(str[7..8], '🐶')
t.eq(str[8..8], '')
t.eq(try str[8..9], error.OutOfBounds)
t.eq(try str[3..1], error.OutOfBounds)

-- concat()
t.eq(str.concat('123'), 'abc🦊xyz🐶123')

-- endsWith()
t.eq(str.endsWith('xyz🐶'), true)
t.eq(str.endsWith('xyz'), false)

-- find()
t.eq(str.find('bc🦊'), 1)
t.eq(str.find('xy'), 4)
t.eq(str.find('bd'), none)
t.eq(str.find('ab'), 0)

-- findAnyRune()
t.eq(str.findAnyRune('a'), 0)
t.eq(str.findAnyRune('🦊'), 3)
t.eq(str.findAnyRune('🦊a'), 0)
t.eq(str.findAnyRune('xy'), 4)
t.eq(str.findAnyRune('ef'), none)

-- findRune()
t.eq(str.findRune(0u'a'), 0)
t.eq(str.findRune(0u'🦊'), 3)
t.eq(str.findRune(0u'x'), 4)
t.eq(str.findRune(0u'd'), none)
t.eq(str.findRune(97), 0)
t.eq(str.findRune(129418), 3)
t.eq(str.findRune(128054), 7)
t.eq(str.findRune(100), none)

-- insert()
t.eq(try str.insert(-1, 'foo'), error.OutOfBounds)
t.eq(str.insert(0, 'foo'), 'fooabc🦊xyz🐶')
t.eq(str.insert(3, 'foo🦊'), 'abcfoo🦊🦊xyz🐶')
t.eq(str.insert(7, 'foo'), 'abc🦊xyzfoo🐶')
t.eq(str.insert(8, 'foo'), 'abc🦊xyz🐶foo')
t.eq(try str.insert(9, 'foo'), error.OutOfBounds)

-- isAscii()
t.eq(str.isAscii(), false)

-- len()
t.eq(str.len(), 8)

-- less()
t.eq(str.less('ac'), true)
t.eq(str.less('aa'), false)

-- lower()
t.eq('AB🦊C'.lower(), 'ab🦊c')

-- repeat()
t.eq(try str.repeat(-1), error.InvalidArgument)
t.eq(str.repeat(0), '')
t.eq(str.repeat(0).isAscii(), true)
t.eq(str.repeat(1), 'abc🦊xyz🐶')
t.eq(str.repeat(1).isAscii(), false)
t.eq(str.repeat(2), 'abc🦊xyz🐶abc🦊xyz🐶')
t.eq(str.repeat(2).isAscii(), false)

-- replace()
t.eq(str.replace('abc🦊', 'foo'), 'fooxyz🐶')
t.eq(str.replace('bc🦊', 'foo'), 'afooxyz🐶')
t.eq(str.replace('bc', 'foo🦊'), 'afoo🦊🦊xyz🐶')
t.eq(str.replace('xy', 'foo'), 'abc🦊fooz🐶')
t.eq(str.replace('xyz🐶', 'foo'), 'abc🦊foo')
t.eq(str.replace('abcd', 'foo'), 'abc🦊xyz🐶')

-- runeAt()
t.eq(try str.runeAt(-1), error.OutOfBounds)
t.eq(str.runeAt(0), 97)
t.eq(str.runeAt(3), 129418)
t.eq(str.runeAt(7), 128054)
t.eq(try str.runeAt(8), error.OutOfBounds)

-- sliceAt().
t.eq(try str.sliceAt(-1), error.OutOfBounds)
t.eq(str.sliceAt(0), 'a')
t.eq(str.sliceAt(0).isAscii(), true)
t.eq(str.sliceAt(3), '🦊')
t.eq(str.sliceAt(3).isAscii(), false)
t.eq(str.sliceAt(7), '🐶')
t.eq(try str.sliceAt(8), error.OutOfBounds)

-- split()
res = 'abc,🐶ab,a'.split(',')
t.eq(res.len(), 3)
t.eq(res[0], 'abc')
t.eq(res[1], '🐶ab')
t.eq(res[2], 'a')

-- startsWith()
t.eq(str.startsWith('abc🦊'), true)
t.eq(str.startsWith('bc🦊'), false)

-- trim()
t.eq(str.trim(#left, 'a'), 'bc🦊xyz🐶')
t.eq(str.trim(#right, '🐶'), 'abc🦊xyz')
t.eq(str.trim(#ends, 'a🐶'), 'bc🦊xyz')

-- upper()
t.eq(str.upper(), 'ABC🦊XYZ🐶')