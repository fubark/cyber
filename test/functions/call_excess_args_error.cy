func foo(a int):
    return a + 3
foo(1, 2)

--cytest: error
--CompileError: Can not find compatible function for call: `foo(_, _)`.
--Functions named `foo` in `main`:
--    func foo(int) dyn
--
--main:3:1:
--foo(1, 2)
--^
--