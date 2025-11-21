type S:
    a int

a := S{a=123} + 123

--cytest: error
--CompileError: Can not find the symbol `+` for `S`.
--
--main:4:6:
--a := S{a=123} + 123
--     ^~~~~~~~~~~~~~
--