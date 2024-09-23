use test

type Number enum:
    case one
    case two
    case three

-- Infer tag literal from dyn call.
func foo(a Number) bool:
    return a == .one
dyn fn = foo
test.eq(fn(.one), true)
test.eq(fn(.two), false)

--cytest: pass