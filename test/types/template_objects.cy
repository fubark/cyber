import test

template(T type)
type Foo:
    var a #T

-- Infer decl type.
var f = [Foo#(String) a: 'abc']
test.eq(f.a, 'abc')

-- Simplified syntax.
f = [Foo#String a: 'abc']
test.eq(f.a, 'abc')

-- Declare with type spec.
var f2 Foo#String = [a: 'abc']
test.eq(f2.a, 'abc')

-- Different variant.
var f3 = [Foo#int a: 123]
test.eq(f3.a, 123)

--cytest: pass