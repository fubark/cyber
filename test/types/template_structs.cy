use test

type Foo[T type] struct:
    a T

    func foo(a T) T:
        var rt_type = T
        var ret T = a
        return ret

    func get(self) T:
        var rt_type = T
        var ret T = self.a
        return ret

    func set(self, a T):
        self.a = a

-- Static function.
test.eq(Foo[int].foo(123), 123)

-- Infer decl type.
var f = Foo[String]{a='abc'}
test.eq(f.a, 'abc')

-- Simplified syntax.
f = Foo[String]{a='abc'}
test.eq(f.a, 'abc')

-- Declare with type spec.
var f2 Foo[String] = .{a='abc'}
test.eq(f2.a, 'abc')

-- Different variant.
var f3 = Foo[int]{a=123}
test.eq(f3.a, 123)

-- Method return type replaced with template param.
var a = f3.get()
test.eq(a, 123)
-- Can be reassigned, indicates `a` is of `int` type.
a = 234    
test.eq(a, 234)

-- Method param type replaced with template param.
f3 = Foo[int]{a=123}
f3.set(234)
test.eq(f3.a, 123)

--cytest: pass