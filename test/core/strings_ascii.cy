use t 'test'

-- Single quote literal.
var str = 'abc'
t.eq(str, 'abc')

-- Multi-line raw string literal.
str = '''abc
abc'''
t.eq(str, "abc\nabc")

-- Multi-line string literal.
str = """abc
abc"""
t.eq(str, "abc\nabc")

str = 'abcxyz'
var lstr = 'aaaaaaaaaaaaaaaamaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaza'

-- index operator
t.eq(str[0], `a`)
t.eq(str[3], `x`)
t.eq(str[5], `z`)
t.eq(try str[-1], error.OutOfBounds)
t.eq(try str[6], error.OutOfBounds)

-- slice operator
t.eq(str[0..], 'abcxyz')
t.eq(str[3..], 'xyz')
t.eq(str[5..], 'z')
t.eq(try str[-1..], error.OutOfBounds)
t.eq(try str[..-1], error.OutOfBounds)
t.eq(try str[7..], error.OutOfBounds)
t.eq(try str[..7], error.OutOfBounds)
t.eq(try str[6..7], error.OutOfBounds)
t.eq(try str[3..1], error.OutOfBounds)
t.eq(str[6..], '')
t.eq(str[..0], '')
t.eq(str[..3], 'abc')
t.eq(str[..5], 'abcxy')
t.eq(str[..6], 'abcxyz')
t.eq(str[0..0], '')
t.eq(str[0..1], 'a')
t.eq(str[3..6], 'xyz')
t.eq(str[5..6], 'z')
t.eq(str[6..6], '')

-- concat()
t.eq(str.concat('123'), 'abcxyz123')
t.eq(str.concat('123').isAscii(), true)
t.eq(str.concat('🦊').isAscii(), false)

-- count()
t.eq(str.count(), 6)

-- endsWith()
t.eq(str.endsWith('xyz'), true)
t.eq(str.endsWith('xy'), false)

-- find()
t.eq(str.find('bc').?, 1)
t.assert(str.find('bd') == none)
t.eq(str.find('ab').?, 0)

-- find() simd 32-byte fixed. Need 'aaa' padding for needle.len = 3 to trigger simd fixed.
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.find('bc').?, 1)
t.assert('abcdefghijklmnopqrstuvwxyz123456aaa'.find('bd') == none)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.find('ab').?, 0)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.find('456').?, 29)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.find('56').?, 30)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.find('6').?, 31)

-- find() simd 32-byte remain.
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.find('7').?, 32)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.find('78').?, 32)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.find('789').?, 32)
t.assert('abcdefghijklmnopqrstuvwxyz123456789'.find('780') == none)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.find('mnopqrstuv').?, 12)
t.assert('abcdefghijklmnopqrstuvwxyz123456789'.find('mnopqrstuw') == none)

-- findAnyRune()
t.eq(str.findAnyRune('a').?, 0)
t.eq(str.findAnyRune('ae').?, 0)
t.eq(str.findAnyRune('ea').?, 0)
t.assert(str.findAnyRune('fe') == none)
t.eq(str.findAnyRune('cd').?, 2)
t.eq(str.findAnyRune('dc').?, 2)
t.eq(str.findAnyRune('cdi').?, 2)

-- findAnyRune() simd 32-byte fixed
t.eq('abcdefghijklmnopqrstuvwxyz123456'.findAnyRune('a').?, 0)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.findAnyRune('m').?, 12)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.findAnyRune('6').?, 31)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.findAnyRune('6m').?, 12)
t.assert('abcdefghijklmnopqrstuvwxyz123456'.findAnyRune('0') == none)
t.assert('abcdefghijklmnopqrstuvwxyz123456'.findAnyRune('07') == none)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.findAnyRune('07m').?, 12)

-- findAnyRune() simd 32-byte remain
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.findAnyRune('7').?, 32)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.findAnyRune('8').?, 33)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.findAnyRune('9').?, 34)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.findAnyRune('98').?, 33)
t.assert('abcdefghijklmnopqrstuvwxyz123456789'.findAnyRune('0') == none)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.findAnyRune('08').?, 33)

-- findRune()
t.eq(str.findRune(`a`).?, 0)
t.eq(str.findRune(`b`).?, 1)
t.eq(str.findRune(`c`).?, 2)
t.assert(str.findRune(`d`) == none)
t.eq(str.findRune(97).?, 0)
t.eq(str.findRune(98).?, 1)
t.eq(str.findRune(99).?, 2)
t.assert(str.findRune(100) == none)

-- findRune() simd
t.eq(lstr.findRune(`a`).?, 0)
t.eq(lstr.findRune(`m`).?, 16)
t.eq(lstr.findRune(`z`).?, 68)

-- insert()
t.eq(try str.insert(-1, 'foo'), error.OutOfBounds)
t.eq(str.insert(0, 'foo'), 'fooabcxyz')
t.eq(str.insert(0, 'foo').isAscii(), true)
t.eq(str.insert(3, 'foo🦊'), 'abcfoo🦊xyz')
t.eq(str.insert(3, 'foo🦊').isAscii(), false)
t.eq(str.insert(5, 'foo'), 'abcxyfooz')
t.eq(str.insert(6, 'foo'), 'abcxyzfoo')
t.eq(try str.insert(7, 'foo'), error.OutOfBounds)

-- isAscii()
t.eq(str.isAscii(), true)

-- len()
t.eq(str.len(), 6)

-- less()
t.eq(str.less('ac'), true)
t.eq(str.less('aa'), false)

-- lower()
t.eq('ABC'.lower(), 'abc')

-- repeat()
t.eq(try str.repeat(-1), error.InvalidArgument)
t.eq(str.repeat(0), '')
t.eq(str.repeat(0).isAscii(), true)
t.eq(str.repeat(1), 'abcxyz')
t.eq(str.repeat(1).isAscii(), true)
t.eq(str.repeat(2), 'abcxyzabcxyz')
t.eq(str.repeat(2).isAscii(), true)
t.eq('abc'.repeat(3), 'abcabcabc')
t.eq('abc'.repeat(4), 'abcabcabcabc')
t.eq('abc'.repeat(5), 'abcabcabcabcabc')

-- replace()
t.eq(str.replace('abc', 'foo'), 'fooxyz')
t.eq(str.replace('bc', 'foo'), 'afooxyz')
t.eq(str.replace('bc', 'foo🦊'), 'afoo🦊xyz')
t.eq(str.replace('bc', 'foo🦊').isAscii(), false)
t.eq(str.replace('xy', 'foo'), 'abcfooz')
t.eq(str.replace('xyz', 'foo'), 'abcfoo')
t.eq(str.replace('abcd', 'foo'), 'abcxyz')

-- sliceAt()
t.eq(str.sliceAt(0), 'a')
t.eq(str.sliceAt(3), 'x')
t.eq(str.sliceAt(5), 'z')
t.eq(try str.sliceAt(6), error.OutOfBounds)

-- split()
t.eqList(''.split(','), List[String].[''])
t.eqList('abc'.split(','), List[String].['abc'])
var res = 'abc,ab,a'.split(',')
t.eq(res.len(), 3)
t.eq(res[0], 'abc')
t.eq(res[1], 'ab')
t.eq(res[2], 'a')

-- startsWith()
t.eq(str.startsWith('abc'), true)
t.eq(str.startsWith('bc'), false)

-- trim()
t.eq(str.trim(.left, 'a'), 'bcxyz')
t.eq(str.trim(.right, 'z'), 'abcxy')
t.eq(str.trim(.ends, 'az'), 'bcxy')

-- upper()
t.eq(str.upper(), 'ABCXYZ')

--cytest: pass