use t 'test'

-- Single quote literal.
s := 'abc'
t.eq(s, 'abc')

-- Multi-line raw string literal.
s = '''abc
abc'''
t.eq(s, "abc\nabc")

-- Multi-line string literal.
s = """abc
abc"""
t.eq(s, "abc\nabc")

s = 'abcxyz'
lstr := 'aaaaaaaaaaaaaaaamaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaza'

-- index operator
t.eq(s[0], 'a')
t.eq(s[3], 'x')
t.eq(s[5], 'z')

-- slice operator
t.eq(s[0..], 'abcxyz')
t.eq(s[3..], 'xyz')
t.eq(s[5..], 'z')
t.eq(s[6..], '')
t.eq(s[0..0], '')
t.eq(s[0..3], 'abc')
t.eq(s[0..5], 'abcxy')
t.eq(s[0..6], 'abcxyz')
t.eq(s[0..0], '')
t.eq(s[0..1], 'a')
t.eq(s[3..6], 'xyz')
t.eq(s[5..6], 'z')
t.eq(s[6..6], '')

-- concat()
t.eq(s.concat('123'), 'abcxyz123')
t.eq(s.concat('123').isAscii(), true)
t.eq(s.concat('🦊').isAscii(), false)

-- count()
t.eq(s.count(), 6)

-- ends_with()
t.eq(s.ends_with('xyz'), true)
t.eq(s.ends_with('xy'), false)

-- index()
t.eq(1, s.index('bc').?)
t.assert(s.index('bd') == none)
t.eq(s.index('ab').?, 0)

-- index() simd 32-byte fixed. Need 'aaa' padding for needle.len = 3 to trigger simd fixed.
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.index('bc').?, 1)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.index('bd'), none)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.index('ab').?, 0)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.index('456').?, 29)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.index('56').?, 30)
t.eq('abcdefghijklmnopqrstuvwxyz123456aaa'.index('6').?, 31)

-- index() simd 32-byte remain.
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index('7').?, 32)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index('78').?, 32)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index('789').?, 32)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index('780'), none)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index('mnopqrstuv').?, 12)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index('mnopqrstuw'), none)

-- index_any([]byte)
t.eq(0, s.index_any({'a'}).?)
t.eq(0, s.index_any({'a', 'e'}).?)
t.eq(0, s.index_any({'e', 'a'}).?)
t.eq(?int(none), s.index_any({'f', 'e'}))
t.eq(2, s.index_any({'c', 'd'}).?)
t.eq(2, s.index_any({'d', 'c'}).?)
t.eq(2, s.index_any({'c', 'd', 'i'}).?)

-- index_any([]byte) simd 32-byte fixed
t.eq('abcdefghijklmnopqrstuvwxyz123456'.index_any({'a'}).?, 0)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.index_any({'m'}).?, 12)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.index_any({'6'}).?, 31)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.index_any({'6', 'm'}).?, 12)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.index_any({'0'}), none)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.index_any({'0', '7'}), none)
t.eq('abcdefghijklmnopqrstuvwxyz123456'.index_any({'0', '7', 'm'}).?, 12)

-- index_any([]byte) simd 32-byte remain
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index_any({'7'}).?, 32)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index_any({'8'}).?, 33)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index_any({'9'}).?, 34)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index_any({'9', '8'}).?, 33)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index_any({'0'}), none)
t.eq('abcdefghijklmnopqrstuvwxyz123456789'.index_any({'0', '8'}).?, 33)

-- index(byte)
t.eq(s.index(byte('a')).?, 0)
t.eq(s.index(byte('b')).?, 1)
t.eq(s.index(byte('c')).?, 2)
t.eq(s.index(byte('d')), none)
t.eq(s.index(byte(97)).?, 0)
t.eq(s.index(byte(98)).?, 1)
t.eq(s.index(byte(99)).?, 2)
t.eq(s.index(byte(100)), none)

-- index(byte) simd
t.eq(lstr.index(byte('a')).?, 0)
t.eq(lstr.index(byte('m')).?, 16)
t.eq(lstr.index(byte('z')).?, 68)

-- insert()
t.eq(s.insert(0, 'foo'), 'fooabcxyz')
t.eq(s.insert(0, 'foo').isAscii(), true)
t.eq(s.insert(3, 'foo🦊'), 'abcfoo🦊xyz')
t.eq(s.insert(3, 'foo🦊').isAscii(), false)
t.eq(s.insert(5, 'foo'), 'abcxyfooz')
t.eq(s.insert(6, 'foo'), 'abcxyzfoo')

-- isAscii()
t.eq(s.isAscii(), true)

-- len()
t.eq(s.len(), 6)

-- less()
t.eq(s.less('ac'), true)
t.eq(s.less('aa'), false)

-- lower()
t.eq('abc', 'ABC'.lower())

-- repeat()
t.eq(s.repeat(0), '')
t.eq(s.repeat(0).isAscii(), true)
t.eq(s.repeat(1), 'abcxyz')
t.eq(s.repeat(1).isAscii(), true)
t.eq(s.repeat(2), 'abcxyzabcxyz')
t.eq(s.repeat(2).isAscii(), true)
t.eq('abc'.repeat(3), 'abcabcabc')
t.eq('abc'.repeat(4), 'abcabcabcabc')
t.eq('abc'.repeat(5), 'abcabcabcabcabc')

-- replace()
t.eq(s.replace('abc', 'foo'), 'fooxyz')
t.eq(s.replace('bc', 'foo'), 'afooxyz')
t.eq(s.replace('bc', 'foo🦊'), 'afoo🦊xyz')
t.eq(s.replace('bc', 'foo🦊').isAscii(), false)
t.eq(s.replace('xy', 'foo'), 'abcfooz')
t.eq(s.replace('xyz', 'foo'), 'abcfoo')
t.eq(s.replace('abcd', 'foo'), 'abcxyz')

-- runeStrAt()
t.eq(s.runeStrAt(0), 'a')
t.eq(s.runeStrAt(3), 'x')
t.eq(s.runeStrAt(5), 'z')

-- split()
t.eq_slice([]str{''}, ''.split(','))
t.eq_slice([]str{'abc'}, 'abc'.split(','))
res := 'abc,ab,a'.split(',')
t.eq(res.len(), 3)
t.eq(res[0], 'abc')
t.eq(res[1], 'ab')
t.eq(res[2], 'a')

-- starts_with()
t.eq(s.starts_with('abc'), true)
t.eq(s.starts_with('bc'), false)

-- trim()
t.eq('bcxyz', s.trimLeft('a'))
t.eq('abcxy', s.trimRight('z'))
t.eq('bcxy', s.trim({'a', 'z'}))

-- upper()
t.eq('ABCXYZ', s.upper())

--cytest: pass