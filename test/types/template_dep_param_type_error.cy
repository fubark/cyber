type Foo[T Any, const Value T] struct:
    a T

f := Foo[int, 10.0]{a=123}

--cytest: error
--CompileError: Expected type `int`. Found `float`.
--
--main:4:15:
--f := Foo[int, 10.0]{a=123}
--              ^~~~
--