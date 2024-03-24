import t 'test'

-- Integers equals.
t.eq(3 == 2, false)
t.eq(3 == 3, true)

-- Strings.
t.eq('foo' == 'bar', false)
t.eq('foo' == 'foo', true)

t.eq(Array('foo') == Array('foo'), true)
t.eq(Array('foo') == Array('bar'), false)

-- Arrays.

-- Heap string equals.
var foo = "$('fo')$('o')"
t.eq(foo == 'bar', false)
foo = "$('fo')$('o')"
t.eq(foo == 'foo', true)

-- Object equals.
type S:
    value any
var s = S{value: 123}
var a = S{value: 123}
t.eq(a == s, false)
a = s
t.eq(a == s, true)

-- Error equals.
t.eq(error.SomeError == error.OtherError, false)
t.eq(error.SomeError == error.SomeError, true)

--cytest: pass