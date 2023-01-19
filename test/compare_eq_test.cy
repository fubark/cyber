import t 'test'

-- Using `is` keyword.
try t.eq(3 is 2, false)
try t.eq(3 is 3, true)

-- Number equals.
try t.eq(3 == 2, false)
try t.eq(3 == 3, true)

-- Const string equals.
try t.eq('foo' == 'bar', false)
try t.eq('foo' == 'foo', true)

-- string matches rawstring contents.
try t.eq('foo' == rawstring('foo'), true)
try t.eq('foo' == rawstring('bar'), false)

-- Heap string equals.
foo = '{'fo'}{'o'}'
try t.eq(foo == 'bar', false)
foo = '{'fo'}{'o'}'
try t.eq(foo == 'foo', true)

-- Object equals.
object S:
  value
s = S{ value: 123 }
a = S{ value: 123 }
try t.eq(a == s, false)
a = s
try t.eq(a == s, true)

-- Error equals.
try t.eq(error(#SomeError) == error(#OtherError), false)
try t.eq(error(#SomeError) == error(#SomeError), true)