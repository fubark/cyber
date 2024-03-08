func foo() void:
    pass

var a = foo()

--cytest: error
--CompileError: Can not find compatible function for call signature: `foo()`. Expects non-void return.
--Functions named `foo` in `main`:
--    func foo() void
--
--main:4:9:
--var a = foo()
--        ^
--