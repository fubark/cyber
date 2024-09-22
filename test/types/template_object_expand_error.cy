type Foo[T type]:
    a T

var f = Foo[string, int]{a='abc'}

--cytest: error
--CompileError: Expected template signature `Foo[type]`.
--
--main:4:9:
--var f = Foo[string, int]{a='abc'}
--        ^
--