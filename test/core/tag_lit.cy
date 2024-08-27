use test

type Number enum:
    case one
    case two
    case three

-- Infer tag literal from dyn method call.
type Object:
    func foo(self, a Number) bool:
        return a == .one
dyn o = test.erase(Object{})
test.eq(o.foo(.one), true)
test.eq(o.foo(.two), false)

-- Infer tag literal from dyn call.
func foo(a Number) bool:
    return a == .one
dyn fn = test.erase(foo)
test.eq(fn(.one), true)
test.eq(fn(.two), false)

--cytest: pass