type S:
    a any

var a = [S a: 123] + 123

--cytest: error
--CompileError: Can not find the symbol `$infix+` in `main.S`.
--
--main:4:20:
--var a = [S a: 123] + 123
--                   ^
--