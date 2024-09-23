use test

-- Functions.
fn sum(a, b, c int) int:
    return a + b + c

test.eq(sum(1, 2, 3), 6)

fn compute(a, b int, c, d string) int:
    return a + b + c.len() + d.len()

test.eq(compute(1, 2, 'abc', 'xyz'), 9)

-- Methods.
type T:
    i int

    fn sum(self, a, b, c int) int:
        return self.i + a + b + c

    fn compute(self, a, b int, c, d string) int:
        return self.i + a + b + c.len() + d.len()

var o = T{i=10}
test.eq(o.sum(1, 2, 3), 16)
test.eq(o.compute(1, 2, 'abc', 'xyz'), 19)

-- Lambdas.
var sum_fn = fn(a, b, c int) int:
    return a + b + c

test.eq(sum_fn(1, 2, 3), 6)

var compute_fn = fn(a, b int, c, d string) int:
    return a + b + c.len() + d.len()

test.eq(compute_fn(1, 2, 'abc', 'xyz'), 9)

--cytest: pass