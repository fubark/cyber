-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

pre = 'abc🦊'
str = '{pre}xyz🐶'
try t.eq(str, 'abc🦊xyz🐶')

upper = '{'abc🦊xyz🐶'}'

-- index operator
try t.eq(str[-1], '🐶')
try t.eq(str[0], 'a')
try t.eq(str[0].isAscii(), true)
try t.eq(str[3], '🦊')
try t.eq(str[3].isAscii(), false)
try t.eq(str[7], '🐶')
try t.eq(str[8], error(#OutOfBounds))

-- slice operator
try t.eq(str[0..], 'abc🦊xyz🐶')
try t.eq(str[4..], 'xyz🐶')
try t.eq(str[7..], '🐶')
try t.eq(str[-1..], '🐶')
try t.eq(str[8..], '')
try t.eq(str[9..], error(#OutOfBounds))
try t.eq(str[-10..], error(#OutOfBounds))
try t.eq(str[..0], '')
try t.eq(str[..4], 'abc🦊')
try t.eq(str[..7], 'abc🦊xyz')
try t.eq(str[..-1], 'abc🦊xyz')
try t.eq(str[..8], 'abc🦊xyz🐶')
try t.eq(str[..9], error(#OutOfBounds))
try t.eq(str[0..0], '')
try t.eq(str[0..1], 'a')
try t.eq(str[4..8], 'xyz🐶')
try t.eq(str[7..8], '🐶')
try t.eq(str[8..8], '')
try t.eq(str[8..9], error(#OutOfBounds))
try t.eq(str[3..1], error(#OutOfBounds))

-- concat()
try t.eq(str.concat('123'), 'abc🦊xyz🐶123')

-- endsWith()
try t.eq(str.endsWith('xyz🐶'), true)
try t.eq(str.endsWith('xyz'), false)

-- find()
try t.eq(str.find('bc🦊'), 1)
try t.eq(str.find('xy'), 4)
try t.eq(str.find('bd'), none)
try t.eq(str.find('ab'), 0)

-- findAnyRune()
try t.eq(str.findAnyRune('a'), 0)
try t.eq(str.findAnyRune('🦊'), 3)
try t.eq(str.findAnyRune('🦊a'), 0)
try t.eq(str.findAnyRune('xy'), 4)
try t.eq(str.findAnyRune('ef'), none)

-- findRune()
try t.eq(str.findRune(0u'a'), 0)
try t.eq(str.findRune(0u'🦊'), 3)
try t.eq(str.findRune(0u'x'), 4)
try t.eq(str.findRune(0u'd'), none)
try t.eq(str.findRune(97), 0)
try t.eq(str.findRune(129418), 3)
try t.eq(str.findRune(128054), 7)
try t.eq(str.findRune(100), none)

-- insert()
try t.eq(str.insert(-1, 'foo'), error(#OutOfBounds))
try t.eq(str.insert(0, 'foo'), 'fooabc🦊xyz🐶')
try t.eq(str.insert(3, 'foo🦊'), 'abcfoo🦊🦊xyz🐶')
try t.eq(str.insert(7, 'foo'), 'abc🦊xyzfoo🐶')
try t.eq(str.insert(8, 'foo'), 'abc🦊xyz🐶foo')
try t.eq(str.insert(9, 'foo'), error(#OutOfBounds))

-- isAscii()
try t.eq(str.isAscii(), false)

-- len()
try t.eq(str.len(), 8)

-- less()
try t.eq(str.less('ac'), true)
try t.eq(str.less('aa'), false)

-- lower()
try t.eq(upper.lower(), 'abc🦊xyz🐶')

-- repeat()
try t.eq(str.repeat(-1), error(#InvalidArgument))
try t.eq(str.repeat(0), '')
try t.eq(str.repeat(0).isAscii(), true)
try t.eq(str.repeat(1), 'abc🦊xyz🐶')
try t.eq(str.repeat(1).isAscii(), false)
try t.eq(str.repeat(2), 'abc🦊xyz🐶abc🦊xyz🐶')
try t.eq(str.repeat(2).isAscii(), false)

-- replace()
try t.eq(str.replace('abc🦊', 'foo'), 'fooxyz🐶')
try t.eq(str.replace('bc🦊', 'foo'), 'afooxyz🐶')
try t.eq(str.replace('bc', 'foo🦊'), 'afoo🦊🦊xyz🐶')
try t.eq(str.replace('xy', 'foo'), 'abc🦊fooz🐶')
try t.eq(str.replace('xyz🐶', 'foo'), 'abc🦊foo')
try t.eq(str.replace('abcd', 'foo'), 'abc🦊xyz🐶')

-- runeAt()
try t.eq(str.runeAt(-1), error(#OutOfBounds))
try t.eq(str.runeAt(0), 97)
try t.eq(str.runeAt(3), 129418)
try t.eq(str.runeAt(7), 128054)
try t.eq(str.runeAt(8), error(#OutOfBounds))

-- sliceAt().
try t.eq(str.sliceAt(-1), error(#OutOfBounds))
try t.eq(str.sliceAt(0), 'a')
try t.eq(str.sliceAt(0).isAscii(), true)
try t.eq(str.sliceAt(3), '🦊')
try t.eq(str.sliceAt(3).isAscii(), false)
try t.eq(str.sliceAt(7), '🐶')
try t.eq(str.sliceAt(8), error(#OutOfBounds))

-- startsWith()
try t.eq(str.startsWith('abc🦊'), true)
try t.eq(str.startsWith('bc🦊'), false)

-- upper()
try t.eq(str.upper(), 'ABC🦊XYZ🐶')