-- CompileError when calling with dynamic value but the recent type can not be casted to the constraint type.
let a = 123
a = 'hello'
foo(a)
func foo(n int):
    pass

--cytest: error
--CompileError: Can not find compatible function for call: `foo(String)`.
--Functions named `foo` in `main`:
--    func foo(int) dyn
--
--main:4:5:
--foo(a)
--    ^
--