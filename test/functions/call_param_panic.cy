let a = foo(123)
a = foo(a)
func foo(a float) dyn:
    return 'foo'

--cytest: error
--panic: Expected type `float`, found `String`.
--
--main:2:9 main:
--a = foo(a)
--        ^
--