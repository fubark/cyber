type S:
    a int

o := S{a=100}
o.b = 200

--cytest: error
--CompileError: Field `b` does not exist in `S`.
--
--main:5:3:
--o.b = 200
--  ^
--