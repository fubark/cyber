type S:
    a float

func foo(): return []

var o = [S a: 123]
o.a = foo()

--cytest: error
--panic: Expected type `float`, found `List`.
--
--main:7:7 main:
--o.a = foo()
--      ^
--