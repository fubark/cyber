use test

-- Without `struct`.
type Foo2[T Any]:
    a T
o := Foo2[int]{a=123}

type Foo[T Any] struct:
    a T

fn Foo[] :: foo(a T) -> T:
    ret := as[T] a
    return ret

fn (&Foo[]) get() -> T:
    ret := as[T] self.a
    return ret

fn (&Foo[]) set(a T):
    self.a = a

-- Static function.
test.eq(Foo[int].foo(123), 123)

-- Infer decl type.
f := Foo[str]{a='abc'}
test.eq(f.a, 'abc')

-- Reassign with infer record syntax.
f = {a='xyz'}
test.eq(f.a, 'xyz')

-- Infer initialize with type spec.
f2 := as[Foo[str]] {a='abc'}
test.eq(f2.a, 'abc')

-- Different variant.
f3 := Foo[int]{a=123}
test.eq(f3.a, 123)

-- Method return type replaced with template param.
a := f3.get()
test.eq(a, 123)
-- Can be reassigned, indicates `a` is of `int` type.
a = 234    
test.eq(a, 234)

-- Method param type replaced with template param.
f3 = Foo[int]{a=123}
f3.set(234)
test.eq(f3.a, 234)

--cytest: pass