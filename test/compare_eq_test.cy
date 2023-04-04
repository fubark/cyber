import t 'test'

-- Using `is` keyword.
t.eq(3 is 2, false)
t.eq(3 is 3, true)

-- Number equals.
t.eq(3 == 2, false)
t.eq(3 == 3, true)

-- Const string equals.
t.eq('foo' == 'bar', false)
t.eq('foo' == 'foo', true)

-- string matches rawstring contents.
t.eq('foo' == toRawstring('foo'), true)
t.eq('foo' == toRawstring('bar'), false)

-- Heap string equals.
foo = '{'fo'}{'o'}'
t.eq(foo == 'bar', false)
foo = '{'fo'}{'o'}'
t.eq(foo == 'foo', true)

-- Object equals.
type S object:
  value
s = S{ value: 123 }
a = S{ value: 123 }
t.eq(a == s, false)
a = s
t.eq(a == s, true)

-- Error equals.
t.eq(error.SomeError == error.OtherError, false)
t.eq(error.SomeError == error.SomeError, true)