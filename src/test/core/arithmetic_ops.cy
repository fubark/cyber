use test

-- Addition.
test.eq(3, 1 + 2)
test.eq(6, 1 + 2 + 3)

-- Subtract.
test.eq(2, 3 - 1)

-- Multiply.
test.eq(12, 3 * 4)

-- Divide.
test.eq(4, 20 / 5)

-- Power.
test.eq(32, 2 ** 5)

-- Modulus.
test.eq(1, 3 % 2)

-- Assign to same var.
a := 0
a = a + 1
test.eq(a, 1)
a = 0
a = 1 + a
test.eq(a, 1)

-- Right function call.
fn foo() -> int:
    return 123
test.eq(1 + foo(), 124)

--cytest: pass