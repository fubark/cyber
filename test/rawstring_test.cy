import t 'test'

str = rawstring('abc🦊xyz🐶')
try t.eq(str, 'abc🦊xyz🐶')

-- index operator
try t.eq(str[-1], error(#InvalidChar))
try t.eq(str[-4], '🐶')
try t.eq(str[0], 'a')
try t.eq(str[0].isAscii(), true)
try t.eq(str[3], '🦊')
try t.eq(str[3].isAscii(), false)
try t.eq(str[4], error(#InvalidChar))
try t.eq(str[10], '🐶')
try t.eq(str[13], error(#InvalidChar))
try t.eq(str[14], error(#OutOfBounds))

-- byteAt()
try t.eq(str.byteAt(-1), error(#OutOfBounds))
try t.eq(str.byteAt(0), 97)
try t.eq(str.byteAt(3), 240)
try t.eq(str.byteAt(4), 159)
try t.eq(str.byteAt(10), 240)
try t.eq(str.byteAt(13), 182)
try t.eq(str.byteAt(14), error(#OutOfBounds))

-- charAt().
try t.eq(str.charAt(-1), error(#OutOfBounds))
try t.eq(str.charAt(0), 'a')
try t.eq(str.charAt(0).isAscii(), true)
try t.eq(str.charAt(3), '🦊')
try t.eq(str.charAt(3).isAscii(), false)
try t.eq(str.charAt(4), error(#InvalidChar))
try t.eq(str.charAt(10), '🐶')
try t.eq(str.charAt(13), error(#InvalidChar))
try t.eq(str.charAt(14), error(#OutOfBounds))

-- codeAt()
try t.eq(str.codeAt(-1), error(#OutOfBounds))
try t.eq(str.codeAt(0), 97)
try t.eq(str.codeAt(3), 129418)
try t.eq(str.codeAt(4), error(#InvalidChar))
try t.eq(str.codeAt(10), 128054)
try t.eq(str.codeAt(13), error(#InvalidChar))
try t.eq(str.codeAt(14), error(#OutOfBounds))

-- concat()
try t.eq(str.concat('123'), 'abc🦊xyz🐶123')

-- endsWith()
try t.eq(str.endsWith('xyz🐶'), true)
try t.eq(str.endsWith('xyz'), false)

-- index()
try t.eq(str.index('bc🦊'), 1)
try t.eq(str.index('xy'), 7)
try t.eq(str.index('bd'), none)
try t.eq(str.index('ab'), 0)

-- indexChar()
try t.eq(str.indexChar('a'), 0)
try t.eq(str.indexChar('🦊'), 3)
try t.eq(str.indexChar('x'), 7)
try t.eq(str.indexChar('d'), none)

-- indexCode()
try t.eq(str.indexCode(97), 0)
try t.eq(str.indexCode(129418), 3)
try t.eq(str.indexCode(128054), 10)
try t.eq(str.indexCode(100), none)

-- insertByte()
try t.eq(str.insertByte(2, 97), 'abac🦊xyz🐶')

-- insert()
try t.eq(str.insert(-1, 'foo'), error(#OutOfBounds))
try t.eq(str.insert(0, 'foo'), 'fooabc🦊xyz🐶')
try t.eq(str.insert(3, 'foo🦊'), 'abcfoo🦊🦊xyz🐶')
try t.eq(str.insert(10, 'foo'), 'abc🦊xyzfoo🐶')
try t.eq(str.insert(14, 'foo'), 'abc🦊xyz🐶foo')
try t.eq(str.insert(15, 'foo'), error(#OutOfBounds))

-- isAscii()
try t.eq(str.isAscii(), false)
try t.eq(rawstring('abc').isAscii(), true)

-- len()
try t.eq(str.len(), 14)

-- less()
try t.eq(str.less('ac'), true)
try t.eq(str.less('aa'), false)

-- lower()
try t.eq(rawstring('AB🦊C').lower(), 'ab🦊c')

-- repeat()
try t.eq(str.repeat(-1), error(#InvalidArgument))
try t.eq(str.repeat(0), '')
try t.eq(str.repeat(1), 'abc🦊xyz🐶')
try t.eq(str.repeat(2), 'abc🦊xyz🐶abc🦊xyz🐶')

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

-- toString()
try t.eq(str.toString(), 'abc🦊xyz🐶')
try t.eq(str.toString().isAscii(), false)
try t.eq(rawstring('abc').toString(), 'abc')
try t.eq(rawstring('abc').isAscii(), true)
try t.eq(rawstring('').insertByte(0, 255).toString(), error(#InvalidChar))

-- upper()
try t.eq(str.upper(), 'ABC🦊XYZ🐶')