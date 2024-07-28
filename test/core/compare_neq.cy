use t 'test'

-- Strings.
t.eq('foo' != 'foo', false)
t.eq('foo' != 'bar', true)

-- Comparing objects.
type S:
    value any
var o = S{value=3}
t.eq(o != S{value=3}, true)
var o2 = o
t.eq(o != o2, false)

-- Compare symbols.
t.eq(symbol.abc != .xyz, true) 
t.eq(symbol.abc != .abc, false)

-- Compare errors.
t.eq(error.SomeError != error.OtherError, true)
t.eq(error.SomeError != error.SomeError, false)

--cytest: pass