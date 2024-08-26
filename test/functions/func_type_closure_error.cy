var b = 123
var a = func(a int) int:
    return a + b

var fn func(int) int = a

--cytest: error
--CompileError: Expected type `func(int) int`, got `Func(int) int`.
--
--main:5:24:
--var fn func(int) int = a
--                       ^
--