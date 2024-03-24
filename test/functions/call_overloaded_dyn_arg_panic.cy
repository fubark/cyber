import test

func foo(a int):
    pass

func foo(a bool):
    pass

var arg = test.erase('123')
foo(arg)

--cytest: error
--panic: Can not find compatible function for call: `foo(String)`.
--Functions named `foo`:
--    func foo(int) dynamic
--    func foo(bool) dynamic
--
--main:10:1 main:
--foo(arg)
--^
--