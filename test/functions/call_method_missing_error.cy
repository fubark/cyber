type S:
    a int

o := S{a=1}
o.foo()

--cytest: error
--CompileError: Can not find the symbol `foo` for `S`.
--
--main:5:3:
--o.foo()
--  ^~~
--