type Choice enum:
    case A int
    case B str

c := Choice.A(123)
c.A

--cytest: error
--CompileError: Type `Choice` does not have a field named `A`.
--
--main:6:3:
--c.A
--  ^
--