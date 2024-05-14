template[T type]
type Foo:
    a #T

var f = Foo[String, int]{a: 'abc'}

--cytest: error
--CompileError: Expected template signature `Foo[type]`.
--
--main:5:9:
--var f = Foo[String, int]{a: 'abc'}
--        ^
--