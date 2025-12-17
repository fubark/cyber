use test

#[jit]
fn fib(n int) -> int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

test.eq(832040, fib(30))
--cytest: pass