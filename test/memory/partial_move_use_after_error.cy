type S:
    a int
    b int

s := S{a=1, b=2}
b := move s.b
s.b

--cytest: error
--CompileError: Member is no longer alive.
--
--main:7:3:
--s.b
--  ^
--