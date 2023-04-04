import t 'test'

-- Using `is not` op.
t.eq(3 is not 2, true)
t.eq(3 is not 3, false)

-- string and rawstring contents.
t.eq('foo' != toRawstring('foo'), false)
t.eq('foo' != toRawstring('bar'), true)

-- Comparing objects.
type S object:
  value
o = S{ value: 3 }
t.eq(o != 123, true)
o2 = o
t.eq(o != o2, false)

-- Compare symbols.
t.eq(#abc != #xyz, true) 
t.eq(#abc != #abc, false)

-- Compare errors.
t.eq(error.SomeError != error.OtherError, true)
t.eq(error.SomeError != error.SomeError, false)