type S:
    a any

var o = S{}
o.foo()

--cytest: error
--CompileError: Can not find the symbol `foo` in `S`.
--
--main:5:3:
--o.foo()
--  ^
--