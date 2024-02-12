import test

type Foo:
    var val int

-- Initialize value.
var a = [+Foo val: 123]
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
func foo(a +Foo):
    test.eq(a.val, 123)
    a.val = 234
a.val = 123
foo(a)
test.eq(a.val, 123)

-- Value choice type.
type Choice enum:
    case a int
    case b float
var c = [+Choice a: 123]
switch c
case .a -> a:
    test.eq(a, 123)
else:
    test.fail()

--cytest: pass