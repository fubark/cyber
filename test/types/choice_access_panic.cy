use test

type Choice enum:
    case A int
    case B String

dyn c = Choice.A(123)
var a = c.A

--cytest: error
--panic: Field not found in value.
--
--main:8:11 main:
--var a = c.A
--          ^
--