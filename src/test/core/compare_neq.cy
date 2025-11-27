use test

-- Strings.
test.eq(false, 'foo' != 'foo')
test.eq(true, 'foo' != 'bar')

-- Comparing objects.
type S:
    value int
o := ^S{value=3}
test.eq(true, o != ^S{value=3})
o2 := o
test.eq(false, o != o2)

-- Compare symbols.
test.eq(true, @abc != @xyz) 
test.eq(false, @abc != @abc)

-- Compare errors.
test.eq(true, error.SomeError != error.OtherError)
test.eq(false, error.SomeError != error.SomeError)

--cytest: pass