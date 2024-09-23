fn foo(a int):
    pass

fn foo(a bool):
    pass

dyn arg = '123'
foo(arg)

--cytest: error
--panic: Expected type `int`, found `string`.
--
--main:8:5 main:
--foo(arg)
--    ^
--