type S object:
    var a float

func foo():
    return 123

var s = [S a: foo()]

--cytest: error
--panic: Initializing `float` field with incompatible type `int`.
--
--main:7:9 main:
--var s = [S a: foo()]
--        ^
--