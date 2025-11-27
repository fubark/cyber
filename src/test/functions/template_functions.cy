use test

-- Inferred template param type.
fn add(a %T, b T) -> T:
    return a + b
test.eq(add(1, 2), 3)
test.eq(add(1.0, 2.0), 3.0)

-- Inferred template param.
fn add2(%T type, a T, b T) -> T:
    return a + b
test.eq(add2(int, 1, 2), 3)
test.eq(add2(float, 1.0, 2.0), 3.0)

-- Explicit template param.
fn add3[T Any](i int, a T, b T) -> T:
    return i + a + b
test.eq(add3[int](1, 2, 3), 6)

-- All compile-time parameters still generates different runtime functions.
fn foo[T Any]() -> int:
    return 123
test.eq(foo[int], foo[int])
test.assert(foo[int] != foo[float])

--cytest: pass