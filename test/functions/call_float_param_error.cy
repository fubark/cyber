fn foo(a float):
    pass
foo(true)

--cytest: error
--CompileError: Can not find compatible function for call: `foo(bool)`.
--Functions named `foo` in `main`:
--    fn foo(float) void
--
--main:3:5:
--foo(true)
--    ^
--