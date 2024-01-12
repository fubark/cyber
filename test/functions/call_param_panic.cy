my a = foo(123)
a = foo(a)
func foo(a float):
    return 'foo'

--cytest: error
--panic: Can not find compatible function for `foo(string) any`.
--Only `func foo(float) dynamic` exists.
--
--main:2:5 main:
--a = foo(a)
--    ^
--