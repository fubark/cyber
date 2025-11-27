fn foo(a int) -> int:
    return a

type Fn1 = fn(int) -> float
func := as[Fn1] foo

--cytest: error
--CompileError: Cannot cast `fn(int) -> int` to `fn(int) -> float`.
--
--@MainPath():5:17:
--func := as[Fn1] foo
--                ^~~
--