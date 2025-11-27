use test

type Choice enum:
    case a int
    case b str

c := Choice.a(123)
_ = c.!b

--cytest: panic
--panic: Expected active choice tag `1`, found `0`.
--
--[trace]
--@MainPath():8:5 main:
--_ = c.!b
--    ^
--