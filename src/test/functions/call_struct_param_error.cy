type Foo:
    a float

fn foo(a Foo):
    pass

foo(123)

--cytest: error
--CompileError: Expected argument `Foo`, found `int`, when calling `fn foo(Foo) -> void`.
--
--@MainPath():7:5:
--foo(123)
--    ^~~
--