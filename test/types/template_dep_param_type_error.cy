type Foo[T type, Value T] struct:
    a T

var f = Foo[int, 10.0]{a=123}

--cytest: error
--CompileError: Expected type `int`. Found `float`.
--
--main:4:18:
--var f = Foo[int, 10.0]{a=123}
--                 ^
--