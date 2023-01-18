-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Single quote literal.
str = 'abc🦊xyz🐶'
try t.eq(str, 'abc🦊xyz🐶')

-- index operator
try t.eq(str[-1], '🐶')
try t.eq(str[0], 'a')
try t.eq(str[0].isAscii(), true)
try t.eq(str[3], '🦊')
try t.eq(str[3].isAscii(), false)
try t.eq(str[7], '🐶')
try t.eq(str[8], error(#OutOfBounds))

-- charAt().
try t.eq(str.charAt(-1), error(#OutOfBounds))
try t.eq(str.charAt(0), 'a')
try t.eq(str.charAt(0).isAscii(), true)
try t.eq(str.charAt(3), '🦊')
try t.eq(str.charAt(3).isAscii(), false)
try t.eq(str.charAt(7), '🐶')
try t.eq(str.charAt(8), error(#OutOfBounds))

-- codeAt()
try t.eq(str.codeAt(-1), error(#OutOfBounds))
try t.eq(str.codeAt(0), 97)
try t.eq(str.codeAt(3), 129418)
try t.eq(str.codeAt(7), 128054)
try t.eq(str.codeAt(8), error(#OutOfBounds))

-- concat()
try t.eq(str.concat('123'), 'abc🦊xyz🐶123')

-- endsWith()
try t.eq(str.endsWith('xyz🐶'), true)
try t.eq(str.endsWith('xyz'), false)

-- index()
try t.eq(str.index('bc🦊'), 1)
try t.eq(str.index('xy'), 4)
try t.eq(str.index('bd'), none)
try t.eq(str.index('ab'), 0)

-- indexChar()
try t.eq(str.indexChar('a'), 0)
try t.eq(str.indexChar('🦊'), 3)
try t.eq(str.indexChar('x'), 4)
try t.eq(str.indexChar('d'), none)

-- indexCharSet()
try t.eq(str.indexCharSet('a'), 0)
try t.eq(str.indexCharSet('🦊'), 3)
try t.eq(str.indexCharSet('🦊a'), 0)
try t.eq(str.indexCharSet('xy'), 4)
try t.eq(str.indexCharSet('ef'), none)

-- indexCode()
try t.eq(str.indexCode(97), 0)
try t.eq(str.indexCode(129418), 3)
try t.eq(str.indexCode(128054), 7)
try t.eq(str.indexCode(100), none)

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
try t.eq('AB🦊C'.lower(), 'ab🦊c')

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

-- startsWith()
try t.eq(str.startsWith('abc🦊'), true)
try t.eq(str.startsWith('bc🦊'), false)

-- upper()
try t.eq(str.upper(), 'ABC🦊XYZ🐶')