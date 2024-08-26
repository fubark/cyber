func foo(a int) int:
    return a

var fn Func(int) float = foo

--cytest: error
--CompileError: Expected type `Func(int) float`, got `func(int) int`.
--
--main:4:26:
--var fn Func(int) float = foo
--                         ^
--