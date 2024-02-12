template(T type)
type Foo:
    var a #T

var a = [Foo a: 123]

--cytest: error
--CompileError: Expected a type symbol. `Foo` is a type template and must be expanded to a type first.
--
--main:5:10:
--var a = [Foo a: 123]
--         ^
--