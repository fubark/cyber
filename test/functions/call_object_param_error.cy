type Foo:
    a float

func foo(a Foo):
    pass

foo(123)

--cytest: error
--CompileError: Can not find compatible function for call: `foo(int)`.
--Functions named `foo` in `main`:
--    func foo(Foo) dynamic
--
--main:7:5:
--foo(123)
--    ^
--