dyn a = foo(123)
a = foo(a)
fn foo(a float) dyn:
    return 'foo'

--cytest: error
--panic: Expected type `float`, found `string`.
--
--main:2:9 main:
--a = foo(a)
--        ^
--