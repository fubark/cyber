import t 'test'

-- Using `is not` op.
try t.eq(3 is not 2, true)
try t.eq(3 is not 3, false)

-- string and rawstring contents.
try t.eq('foo' != rawstring('foo'), false)
try t.eq('foo' != rawstring('bar'), true)

-- Comparing objects.
object S:
  value
o = S{ value: 3 }
try t.eq(o != 123, true)
o2 = o
try t.eq(o != o2, false)

-- Compare tag literal.
try t.eq(#abc != #xyz, true) 
try t.eq(#abc != #abc, false)

-- Compare errors.
try t.eq(error(#SomeError) != error(#OtherError), true)
try t.eq(error(#SomeError) != error(#SomeError), false)