use test

-- Inferred template param.
func add(a #T, b T) T:
    return a + b
test.eq(add(1, 2), 3)
test.eq(add(1.0, 2.0), 3.0)

-- Expand template params.
test.eq(add[int](1, 2), 3)
test.eq(add[float](1, 2), 3.0)

-- Explicit template param.
func add2(#T type, a T, b T) T:
    return a + b
test.eq(add2(int, 1, 2), 3)
test.eq(add2(float, 1.0, 2.0), 3.0)

-- Expand template params.
test.eq(add2[int](1, 2), 3)
test.eq(add2[float](1, 2), 3.0)

-- Infer param declared after runtime param.
func add3(i int, a #T, b T) T:
    return i + a + b
test.eq(add3(1, 2, 3), 6)

-- All compile-time parameters still generates different runtime functions.
func foo(#T type) int:
    return 123
test.eq(foo[int], foo[int])
test.assert(foo[int] != foo[float])

--cytest: pass