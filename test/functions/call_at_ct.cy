use test

-- fn GetType(a int) type:
--     if a == 0:
--         return bool
--     else:
--         return string

-- var a GetType(0) = true
-- test.eq(a, true)

-- var b GetType(1) = 'abc'
-- test.eq(b, 'abc')

fn fib(n int) -> int:
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

-- -- TODO: Should be testing inline evaluation instead: #{fib(10)}
-- var c = $eval(fib(10))
-- test.eq(c, 55)

--cytest: pass