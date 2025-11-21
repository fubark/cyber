type Foo[T type]:
    a T

type Bar:
    a Foo

--cytest: error
--CompileError: Expected a type symbol. `Foo` is a type template and must be expanded to a type first.
--
--main:5:7:
--    a Foo
--      ^~~
--main:4:1:
--type Bar:
--^~~~~~~~~
--