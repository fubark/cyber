func foo(a int) int:
    return a

var fn func(int) float = foo

--cytest: error
--CompileError: Expected type `func(int) float`, got `func(int) int`.
--
--main:4:26:
--var fn func(int) float = foo
--                         ^
--