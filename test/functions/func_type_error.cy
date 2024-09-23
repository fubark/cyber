fn foo(a int) int:
    return a

var func fn(int) float = foo

--cytest: error
--CompileError: Expected type `fn(int) float`, got `fn(int) int`.
--
--main:4:26:
--var func fn(int) float = foo
--                         ^
--