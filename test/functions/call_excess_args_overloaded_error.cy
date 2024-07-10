func foo():
    return 1

func foo(n int):
    return n

foo(1, 2)

--cytest: error
--CompileError: Can not find compatible function for call: `foo(_, _)`.
--Functions named `foo` in `main`:
--    func foo() dyn
--    func foo(int) dyn
--
--main:7:1:
--foo(1, 2)
--^
--