template[T type]
type Foo:
    a #T

type Bar:
    a Foo

--cytest: error
--CompileError: Expected a type symbol. `Foo` is a type template and must be expanded to a type first.
--
--main:6:7:
--    a Foo
--      ^
--