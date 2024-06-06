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

--cytest: pass