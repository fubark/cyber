use test
use math
use meta

-- Default notation.
test.eq(1.0, 1.0)
test.eq(-1.0, -1.0)
test.eq(1.0, -(-1.0))

-- Scientific notation.
test.eq(123.0, 1.23e2)
test.eqNear(1.23, 123e-2)

-- float.$init()
test.eq(100.0, float(100))
test.eq(100.1, float(100.1))
if meta.is_vm_target():
    test.eq(100.0, float('100'))
    test.eq(100.1, float('100.1'))
test.eq(1.0, float(true))
test.eq(0.0, float(false))

-- Infinity.
test.eq(math.inf, 1.0 / 0)

-- NaN.
test.eq(true, math.isNaN(0.0 * (1.0 / 0)))

--cytest: pass