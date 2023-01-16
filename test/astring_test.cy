import t 'test'

pre = 'abc'
str = '{pre}xyz'
try t.eq(str, 'abcxyz')

-- index operator
try t.eq(str[-1], 'z')
try t.eq(str[0], 'a')
try t.eq(str[3], 'x')
try t.eq(str[5], 'z')
try t.eq(str[6], error(#OutOfBounds))

-- charAt()
try t.eq(str.charAt(-1), error(#OutOfBounds))
try t.eq(str.charAt(0), 'a')
try t.eq(str.charAt(3), 'x')
try t.eq(str.charAt(5), 'z')
try t.eq(str.charAt(6), error(#OutOfBounds))

-- codeAt()
try t.eq(str.codeAt(-1), error(#OutOfBounds))
try t.eq(str.codeAt(0), 97)
try t.eq(str.codeAt(3), 120)
try t.eq(str.codeAt(5), 122)
try t.eq(str.codeAt(6), error(#OutOfBounds))

-- concat()
try t.eq(str.concat('123'), 'abcxyz123')
try t.eq(str.concat('123').isAscii(), true)
try t.eq(str.concat('🦊').isAscii(), false)

-- endsWith()
try t.eq(str.endsWith('xyz'), true)
try t.eq(str.endsWith('xy'), false)

-- index()
try t.eq(str.index('bc'), 1)
try t.eq(str.index('bd'), none)
try t.eq(str.index('ab'), 0)

-- indexChar()
try t.eq(str.indexChar('a'), 0)
try t.eq(str.indexChar('b'), 1)
try t.eq(str.indexChar('c'), 2)
try t.eq(str.indexChar('d'), none)

-- indexChar() simd
lstr = '{'aaaaaaaaaaaaaaaamaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaza'}'
try t.eq(lstr.indexChar('a'), 0)
try t.eq(lstr.indexChar('m'), 16)
try t.eq(lstr.indexChar('z'), 68)

-- indexCode()
try t.eq(str.indexCode(97), 0)
try t.eq(str.indexCode(98), 1)
try t.eq(str.indexCode(99), 2)
try t.eq(str.indexCode(100), none)

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
try t.eq('ABC'.lower(), 'abc')

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

-- startsWith()
try t.eq(str.startsWith('abc'), true)
try t.eq(str.startsWith('bc'), false)

-- upper()
try t.eq(str.upper(), 'ABCXYZ')