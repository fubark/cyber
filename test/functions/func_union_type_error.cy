fn foo(a int) -> int:
    return a

type Fn1 = fn(int) -> float
func := as[Func[Fn1]] foo

--cytest: error
--CompileError: Cannot cast `fn(int) -> int` to `Func[fn(int) -> float]`.
--
--main:5:23:
--func := as[Func[Fn1]] foo
--                      ^~~
--