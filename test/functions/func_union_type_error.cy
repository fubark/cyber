fn foo(a int) int:
    return a

var func Fn(int) float = foo

--cytest: error
--CompileError: Expected type `Fn(int) float`, got `fn(int) int`.
--
--main:4:26:
--var func Fn(int) float = foo
--                         ^
--