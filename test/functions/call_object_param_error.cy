type Foo:
    a float

func foo(a Foo):
    pass

foo(123)

--cytest: error
--CompileError: Can not find compatible function for call signature: `foo(int) any`.
--Functions named `foo` in `main`:
--    func foo(Foo) dynamic
--
--main:7:1:
--foo(123)
--^
--