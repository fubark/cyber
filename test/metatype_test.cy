import t 'test'

-- id()
t.eq(typeof(none).id(), 0)
t.eq(typeof(true).id(), 1)
t.eq(typeof(false).id(), 1)
t.eq(typeof(error.err).id(), 2)
t.eq(typeof('abc').id(), 3)
t.eq(typeof('abc🦊').id(), 4)
t.eq(typeof(#abc).id(), 6)
t.eq(typeof(int(123)).id(), 7)
t.eq(typeof(123).id(), 8)
t.eq(typeof([]).id(), 9)
t.eq(typeof({}).id(), 11)