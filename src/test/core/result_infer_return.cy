use test

-- Infer error return.
fn fail() -> !int:
    return error.Fail
test.eq(error.Fail, fail().unwrap_error())

-- Infer error void return.
fn fail_void() -> !void:
    return error.Fail
test.eq(error.Fail, fail_void().unwrap_error())

-- Infer value return.
fn succeed() -> !int:
    return 123
test.eq(123, succeed()!)

-- Infer void return.
fn succeed_void() -> !void:
    return
test.eq(_, succeed_void()!)

-- Infer implicit void return.
fn succeed_void2() -> !void:
    pass
test.eq(_, succeed_void2()!)

-- Infer `none`.
fn infer_none() -> !?int:
    return none
test.eq(?int(none), infer_none()!)

-- Infer `some`.
fn infer_some() -> !?int:
    return 123
test.eq(123, infer_some()!.?)

-- -- Infer `some` init literal.
-- type Foo:
--     a int
-- fn infer_some_lit() -> !?Foo:
--     return {a = 123}
-- test.eq(123, infer_some_lit()!.?)

--cytest: pass