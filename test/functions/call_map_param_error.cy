func foo(a Map):
    pass
foo(123)

--cytest: error
--CompileError: Can not find compatible function for call signature: `foo(int) any`.
--Functions named `foo` in `main`:
--    func foo(Map) dynamic
--
--main:3:1:
--foo(123)
--^
--