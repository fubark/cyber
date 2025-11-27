use test

-- Strings.
test.eq(false, 'foo' == 'bar')
test.eq(true, 'foo' == 'foo')

-- Reference equals.
type S:
    value int
s := ^S{value=123}
a := ^S{value=123}
test.eq(false, a == s)
a = s
test.eq(true, a == s)

-- Error equals.
test.eq(false, error.SomeError == error.OtherError)
test.eq(true, error.SomeError == error.SomeError)

--cytest: pass