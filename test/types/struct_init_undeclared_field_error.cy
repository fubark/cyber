type S:
    a Object

o := S{b=100}

--cytest: error
--CompileError: Field `b` does not exist in `S`.
--
--main:4:8:
--o := S{b=100}
--       ^
--