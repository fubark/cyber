use test

fn foo(a int) -> int:
    return a

type Fn1 = fn(int) -> int

-- func union can be assigned with func sym.
func := as[Func[Fn1]] foo
test.eq(func(123), 123)

-- func union can be assigned with lamda.
func = fn(a int) -> int:
    return a * 2
test.eq(func(10), 20)

-- func union can be assigned with closure.
a := ^123
func = fn(b int) -> int:
    return a.* + b
test.eq(func(1), 124)

--cytest: pass