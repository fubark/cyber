use test

template[T type]
type Foo:
    a #T

    func get() #T:
        return self.a

    func set(a #T):
        self.a = a

-- Infer decl type.
var f = Foo[String]{a: 'abc'}
test.eq(f.a, 'abc')

-- Simplified syntax.
f = Foo[String]{a: 'abc'}
test.eq(f.a, 'abc')

-- Declare with type spec.
var f2 Foo[String] = {a: 'abc'}
test.eq(f2.a, 'abc')

-- Different variant.
var f3 = Foo[int]{a: 123}
test.eq(f3.a, 123)

-- Method return type replaced with template param.
var a = f3.get()
test.eq(a, 123)
-- Can be reassigned, indicates `a` is of `int` type.
a = 234    
test.eq(a, 234)

-- Method param type replaced with template param.
f3 = Foo[int]{a: 123}
f3.set(234)
test.eq(f3.a, 234)

--cytest: pass