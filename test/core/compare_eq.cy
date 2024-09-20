use t 'test'

-- Strings.
t.eq('foo' == 'bar', false)
t.eq('foo' == 'foo', true)

-- Reference equals.
type S:
    value any
var s = ^S{value=123}
var a = ^S{value=123}
t.eq(a == s, false)
a = s
t.eq(a == s, true)

-- Error equals.
t.eq(error.SomeError == error.OtherError, false)
t.eq(error.SomeError == error.SomeError, true)

--cytest: pass