type S:
    a float

func foo() dyn: return 'abc'

var o = S{a=123}
o.a = foo()

--cytest: error
--panic: Expected type `float`, found `String`.
--
--main:7:7 main:
--o.a = foo()
--      ^
--