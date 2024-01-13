type S:
    var a float

func foo():
    return 123

var s = [S a: none]

--cytest: error
--CompileError: Expected type `float`, got `none`.
--
--main:7:15:
--var s = [S a: none]
--              ^
--