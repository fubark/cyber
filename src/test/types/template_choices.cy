use test

type Foo[T Any] enum:
    case a str
    case b T

-- Infer decl type.
f := Foo[int].b(123)
switch f:
    case .b |b|:
        test.eq(b, 123)
    else:
        test.fail()

-- Different variant.
f2 := Foo[str].b('abc')
switch f2:
    case .b |b|:
        test.eq(b, 'abc')
    else:
        test.fail()

--cytest: pass