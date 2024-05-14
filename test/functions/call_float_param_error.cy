func foo(a float):
    pass
foo(true)

--cytest: error
--CompileError: Can not find compatible function for call: `foo(bool)`.
--Functions named `foo` in `main`:
--    func foo(float) dyn
--
--main:3:5:
--foo(true)
--    ^
--