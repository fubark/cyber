func GetType(a int) type:
    if a == 0:
        return bool
    else:
        return string

var i = 0
GetType#(i)

--cytest: error
--CompileError: Expected compile-time argument.
--
--main:8:10:
--GetType#(i)
--         ^
--