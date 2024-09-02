use test

-- Functions.
func sum(a, b, c int) int:
    return a + b + c

test.eq(sum(1, 2, 3), 6)

func compute(a, b int, c, d String) int:
    return a + b + c.len() + d.len()

test.eq(compute(1, 2, 'abc', 'xyz'), 9)

-- Methods.
type T:
    i int

    func sum(self, a, b, c int) int:
        return self.i + a + b + c

    func compute(self, a, b int, c, d String) int:
        return self.i + a + b + c.len() + d.len()

var o = T{i=10}
test.eq(o.sum(1, 2, 3), 16)
test.eq(o.compute(1, 2, 'abc', 'xyz'), 19)

-- Lambdas.
var sum_fn = func(a, b, c int) int:
    return a + b + c

test.eq(sum_fn(1, 2, 3), 6)

var compute_fn = func(a, b int, c, d String) int:
    return a + b + c.len() + d.len()

test.eq(compute_fn(1, 2, 'abc', 'xyz'), 9)

--cytest: pass