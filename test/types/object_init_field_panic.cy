type S:
    a float

func foo():
    return 123

var s = [S a: foo()]

--cytest: error
--panic: Expected type `float`, found `int`.
--
--main:7:15 main:
--var s = [S a: foo()]
--              ^
--