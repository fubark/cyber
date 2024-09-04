func foo(a int):
    pass

func foo(a bool):
    pass

dyn arg = '123'
foo(arg)

--cytest: error
--panic: Can not find compatible function for call: `foo(String)`.
--Functions named `foo`:
--    func foo(int) void
--    func foo(bool) void
--
--main:8:1 main:
--foo(arg)
--^
--