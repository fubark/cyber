import t 'test'

-- Integers.
t.eq(3 != 2, true)
t.eq(3 != 3, false)

-- Strings.
t.eq('foo' != 'foo', false)
t.eq('foo' != 'bar', true)

-- Arrays.
t.eq(Array('foo') != Array('foo'), false)
t.eq(Array('foo') != Array('bar'), true)

-- Comparing objects.
type S:
    value any
var o = S{value: 3}
t.eq(o != S{value: 3}, true)
var o2 = o
t.eq(o != o2, false)

-- Compare symbols.
t.eq(.abc != .xyz, true) 
t.eq(.abc != .abc, false)

-- Compare errors.
t.eq(error.SomeError != error.OtherError, true)
t.eq(error.SomeError != error.SomeError, false)

--cytest: pass