type S object:
    var a

var o = [S:]
o.foo()

--cytest: error
--CompileError: Can not find the symbol `foo` in `main.S`.
--
--main:5:3:
--o.foo()
--  ^
--