use test

type Number enum:
    case one
    case two
    case three

-- Infer tag literal from dyn call.
fn foo(a Number) bool:
    return a == .one
dyn func = foo
test.eq(func(.one), true)
test.eq(func(.two), false)

--cytest: pass