func foo(a Fiber):
    pass
foo(true)

--cytest: error
--CompileError: Can not find compatible function for call signature: `foo(bool) any`.
--Functions named `foo` in `main`:
--    func foo(Fiber) dynamic
--
--main:3:1:
--foo(true)
--^
--