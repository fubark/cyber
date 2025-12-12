use test

fn foo() -> !int:
    a := bar()!
    return a

fn bar() -> !int:
    return error.Fail

test.eq(error.Fail, foo().unwrap_error())

-- Propagate for error return.
fn errorReturn() -> error:
    _ = bar()!
    return error.NotThisError
test.eq(error.Fail, errorReturn())

--cytest: pass