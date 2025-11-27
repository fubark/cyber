use test

-- Initialize value.
res := as[!int] 123
test.eq(123, res!)

-- Initialize value, implicitly.
res = 234
test.eq(234, res!)

-- Initialize error.
res = as[!int] error.Invalid
test.eq(error.Invalid, res.unwrapError())

-- Initialize error, implicitly.
res = error.Invalid
test.eq(error.Invalid, res.unwrapError())

-- Unwrap value.
fn ok() -> !str:
    res := Result[int](123)
    return str(res!)
test.eq('123', ok()!)

-- Unwrap error and propagate.
fn fail() -> !str:
    res := Result[int](error.Invalid)
    return str(res!)
test.eq(error.Invalid, fail().unwrapError())

-- !else unwraps value.
res = 123
test.eq(123, res !else 0)

-- !else defaults.
res = error.Invalid
test.eq(0, res !else 0)

-- !else block unwraps value.
res = 123
act := res !else:
    panic('Unexpected')
test.eq(res, act)

-- !else block branches to error case.
fn foo() -> int:
    res := Result[int](error.Invalid)
    return res !else:
        return 0
test.eq(0, foo())

-- !else block branches and captures error.
fn foo2() -> int:
    res := Result[int](error.Invalid)
    return res !else |err|:
        test.eq(error.Invalid, err)
        return 0
test.eq(0, foo2())

--cytest: pass