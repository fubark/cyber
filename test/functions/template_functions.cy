use test

template[T type]
func add(a T, b T) T:
    return a + b

-- Expand template params.
test.eq(add[int](1, 2), 3)
test.eq(add[float](1, 2), 3.0)

-- Infer template params.
test.eq(add(1, 2), 3)
test.eq(add(1.0, 2.0), 3.0)

--cytest: pass