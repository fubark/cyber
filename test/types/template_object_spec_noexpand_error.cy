template(T type)
type Foo:
    var a #T

type Bar:
    var a Foo

--cytest: error
--CompileError: Expected a type symbol. `Foo` is a type template and must be expanded to a type first.
--
--main:6:11:
--    var a Foo
--          ^
--