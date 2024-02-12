import test

type Foo struct:
    var val int

-- Initialize value.
var a = [Foo val: 123]
test.eq(a.val, 123)

-- Set field.
a.val = 234
test.eq(a.val, 234)

-- Assignment copies value.
var b = a
a.val = 123
test.eq(b.val, 234)
test.eq(a.val, 123)

-- Pass call argument by value.
-- Original value remains unchanged.
func foo(a Foo):
    test.eq(a.val, 123)
    a.val = 234
a.val = 123
foo(a)
test.eq(a.val, 123)

--cytest: pass