fn foo(a int):
    pass

fn foo(a bool):
    pass

dyn arg = '123'
foo(arg)

--cytest: error
--panic: Can not find compatible function for call: `foo(string)`.
--Functions named `foo`:
--    fn foo(int) void
--    fn foo(bool) void
--
--main:8:1 main:
--foo(arg)
--^
--