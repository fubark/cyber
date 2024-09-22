use test

type Choice enum:
    case a int
    case b string

var c = Choice.a(123)
c.!b

--cytest: error
--panic: Expected active choice tag `1`, found `0`.
--
--main:8:1 main:
--c.!b
--^
--