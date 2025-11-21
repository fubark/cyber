use test

type S:
    a float

o := S{}

--cytest: error
--CompileError: Initialization requires the field `a`.
--
--main:6:7:
--o := S{}
--      ^~
--