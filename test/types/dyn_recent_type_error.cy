-- CompileError when calling with dynamic value but the recent type can not be casted to the constraint type.
my a = 123
a = 'hello'
foo(a)
func foo(n int):
    pass

--cytest: error
--CompileError: Can not find compatible function for call signature: `foo(dyn String)`.
--Functions named `foo` in `main`:
--    func foo(int) dynamic
--
--main:4:1:
--foo(a)
--^
--