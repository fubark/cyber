b := ^123
a := fn(a int) -> int:
    return a + b.*

type Fn1 = fn(int) -> int
func := as[Fn1] a

--cytest: error
--CompileError: Cannot cast `Func[fn(int) -> int]` to `fn(int) -> int`.
--
--main:6:17:
--func := as[Fn1] a
--                ^
--