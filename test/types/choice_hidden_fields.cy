import test

type Choice enum:
    case A int
    case B String

my c = test.erase([Choice A: 123])
test.eq(c.tag, none)
test.eq(c.A, none)
test.eq(c.B, none)

--cytest: pass