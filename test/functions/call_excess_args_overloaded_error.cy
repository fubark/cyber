func foo():
    return 1

func foo(n):
    return n

foo(1, 2)

--cytest: error
--CompileError: Can not find compatible function for call signature: `foo(int, int) any`.
--Functions named `foo` in `main`:
--    func foo() dynamic
--    func foo(dynamic) dynamic
--
--main:7:1:
--foo(1, 2)
--^
--