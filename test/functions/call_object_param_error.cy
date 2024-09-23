type Foo:
    a float

fn foo(a Foo):
    pass

foo(123)

--cytest: error
--CompileError: Can not find compatible function for call: `foo(int)`.
--Functions named `foo` in `main`:
--    fn foo(Foo) void
--
--main:7:5:
--foo(123)
--    ^
--