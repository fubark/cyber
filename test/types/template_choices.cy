use test

type Foo[T type] enum:
    case a string
    case b T

-- Infer decl type.
var f = Foo[int].b(123)
switch f
case .b -> b:
    test.eq(b, 123)
else:
    test.fail()

-- Different variant.
var f2 = Foo[string].b('abc')
switch f2
case .b -> b:
    test.eq(b, 'abc')
else:
    test.fail()

--cytest: pass