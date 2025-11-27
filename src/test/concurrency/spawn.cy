use test

-- No args.
test.eq(123, spawn(entry, {}).await())

-- Sending primitive.
test.eq(45, spawn(entry2, {10}).await())

-- Sending `str`.
test.eq('xyz', spawn(entry3, {'abc'}).await())

fn entry() -> int:
    return 123

fn entry2(n int) -> int:
    sum := 0
    for 0..n |i|:
        sum += i
    return sum

fn entry3(s str) -> str:
    return 'xyz'

--cytest: pass