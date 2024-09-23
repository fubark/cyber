var b = 123
var a = fn(a int) int:
    return a + b

var func fn(int) int = a

--cytest: error
--CompileError: Expected type `fn(int) int`, got `Fn(int) int`.
--
--main:5:24:
--var func fn(int) int = a
--                       ^
--