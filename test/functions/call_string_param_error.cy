func foo(a String):
    pass
foo(123)

--cytest: error
--CompileError: Can not find compatible function for call signature: `foo(int) any`.
--Functions named `foo` in `main`:
--    func foo(String) dynamic
--
--main:3:1:
--foo(123)
--^
--