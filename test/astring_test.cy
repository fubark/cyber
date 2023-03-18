-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

pre = 'abc'
str = '{pre}xyz'
try t.eq(str, 'abcxyz')

lstr = '{'aaaaaaaaaaaaaaaamaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaza'}'
ustr = '{'ABCXYZ'}'

-- index operator
try t.eq(str[-1], 'z')
try t.eq(str[0], 'a')
try t.eq(str[3], 'x')
try t.eq(str[5], 'z')
try t.eq(str[6], error(#OutOfBounds))

-- slice operator
try t.eq(str[0..], 'abcxyz')
try t.eq(str[3..], 'xyz')
try t.eq(str[5..], 'z')
try t.eq(str[-1..], 'z')
try t.eq(str[6..], '')
try t.eq(str[7..], error(#OutOfBounds))
try t.eq(str[-10..], error(#OutOfBounds))
try t.eq(str[..0], '')
try t.eq(str[..3], 'abc')
try t.eq(str[..5], 'abcxy')
try t.eq(str[..-1], 'abcxy')
try t.eq(str[..6], 'abcxyz')
try t.eq(str[..7], error(#OutOfBounds))
try t.eq(str[0..0], '')
try t.eq(str[0..1], 'a')
try t.eq(str[3..6], 'xyz')
try t.eq(str[5..6], 'z')
try t.eq(str[6..6], '')
try t.eq(str[6..7], error(#OutOfBounds))
try t.eq(str[3..1], error(#OutOfBounds))

-- concat()
try t.eq(str.concat('123'), 'abcxyz123')
try t.eq(str.concat('123').isAscii(), true)
try t.eq(str.concat('🦊').isAscii(), false)

-- endsWith()
try t.eq(str.endsWith('xyz'), true)
try t.eq(str.endsWith('xy'), false)

-- find()
try t.eq(str.find('bc'), 1)
try t.eq(str.find('bd'), none)
try t.eq(str.find('ab'), 0)

-- findAnyRune()
try t.eq(str.findAnyRune('a'), 0)
try t.eq(str.findAnyRune('ae'), 0)
try t.eq(str.findAnyRune('fe'), none)
try t.eq(str.findAnyRune('cd'), 2)

-- findRune()
try t.eq(str.findRune(0u'a'), 0)
try t.eq(str.findRune(0u'b'), 1)
try t.eq(str.findRune(0u'c'), 2)
try t.eq(str.findRune(0u'd'), none)
try t.eq(str.findRune(97), 0)
try t.eq(str.findRune(98), 1)
try t.eq(str.findRune(99), 2)
try t.eq(str.findRune(100), none)

-- findRune() simd
try t.eq(lstr.findRune(0u'a'), 0)
try t.eq(lstr.findRune(0u'm'), 16)
try t.eq(lstr.findRune(0u'z'), 68)

-- insert()
try t.eq(str.insert(-1, 'foo'), error(#OutOfBounds))
try t.eq(str.insert(0, 'foo'), 'fooabcxyz')
try t.eq(str.insert(0, 'foo').isAscii(), true)
try t.eq(str.insert(3, 'foo🦊'), 'abcfoo🦊xyz')
try t.eq(str.insert(3, 'foo🦊').isAscii(), false)
try t.eq(str.insert(5, 'foo'), 'abcxyfooz')
try t.eq(str.insert(6, 'foo'), 'abcxyzfoo')
try t.eq(str.insert(7, 'foo'), error(#OutOfBounds))

-- isAscii()
try t.eq(str.isAscii(), true)

-- len()
try t.eq(str.len(), 6)

-- less()
try t.eq(str.less('ac'), true)
try t.eq(str.less('aa'), false)

-- lower()
try t.eq(ustr.lower(), 'abcxyz')

-- repeat()
try t.eq(str.repeat(-1), error(#InvalidArgument))
try t.eq(str.repeat(0), '')
try t.eq(str.repeat(0).isAscii(), true)
try t.eq(str.repeat(1), 'abcxyz')
try t.eq(str.repeat(1).isAscii(), true)
try t.eq(str.repeat(2), 'abcxyzabcxyz')
try t.eq(str.repeat(2).isAscii(), true)

-- replace()
try t.eq(str.replace('abc', 'foo'), 'fooxyz')
try t.eq(str.replace('bc', 'foo'), 'afooxyz')
try t.eq(str.replace('bc', 'foo🦊'), 'afoo🦊xyz')
try t.eq(str.replace('bc', 'foo🦊').isAscii(), false)
try t.eq(str.replace('xy', 'foo'), 'abcfooz')
try t.eq(str.replace('xyz', 'foo'), 'abcfoo')
try t.eq(str.replace('abcd', 'foo'), 'abcxyz')

-- runeAt()
try t.eq(str.runeAt(-1), error(#OutOfBounds))
try t.eq(str.runeAt(0), 97)
try t.eq(str.runeAt(3), 120)
try t.eq(str.runeAt(5), 122)
try t.eq(str.runeAt(6), error(#OutOfBounds))

-- sliceAt()
try t.eq(str.sliceAt(-1), error(#OutOfBounds))
try t.eq(str.sliceAt(0), 'a')
try t.eq(str.sliceAt(3), 'x')
try t.eq(str.sliceAt(5), 'z')
try t.eq(str.sliceAt(6), error(#OutOfBounds))

-- split()
res = string('abc,ab,a').split(',')
try t.eq(res.len(), 3)
try t.eq(res[0], 'abc')
try t.eq(res[1], 'ab')
try t.eq(res[2], 'a')

-- startsWith()
try t.eq(str.startsWith('abc'), true)
try t.eq(str.startsWith('bc'), false)

-- trim()
try t.eq(str.trim(#left, 'a'), 'bcxyz')
try t.eq(str.trim(#right, 'z'), 'abcxy')
try t.eq(str.trim(#ends, 'az'), 'bcxy')

-- upper()
try t.eq(str.upper(), 'ABCXYZ')