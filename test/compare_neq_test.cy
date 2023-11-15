import t 'test'

-- Using `is not` op.
t.eq(3 is not 2, true)
t.eq(3 is not 3, false)

-- strings.
t.eq('foo' != 'foo', false)
t.eq('foo' != 'bar', true)

-- arrays.
t.eq(array('foo') != array('foo'), false)
t.eq(array('foo') != array('bar'), true)

-- Comparing objects.
type S object:
    var value
var o = [S value: 3]
t.eq(o != 123, true)
var o2 = o
t.eq(o != o2, false)

-- Compare symbols.
t.eq(.abc != .xyz, true) 
t.eq(.abc != .abc, false)

-- Compare errors.
t.eq(error.SomeError != error.OtherError, true)
t.eq(error.SomeError != error.SomeError, false)