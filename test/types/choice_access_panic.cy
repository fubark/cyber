use test

type Choice enum:
    case A int
    case B String

dyn c = test.erase(Choice.A(123))
var a = c.A

--cytest: error
--panic: Missing field in object.
--
--main:8:11 main:
--var a = c.A
--          ^
--