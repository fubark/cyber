type Foo[T type]:
    a T

a := Foo{a=123}

--cytest: error
--CompileError: Expected a type, found type template. Expand `Foo` to a type.
--
--@MainPath():4:6:
--a := Foo{a=123}
--     ^~~
--