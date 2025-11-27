use t 'test'

type Animal enum:
    case bear
    case tiger

-- enum to int.
n := Animal.tiger
t.eq(1, as n)

-- Using enum declared afterwards.
n2 := Animal2.tiger
t.eq(1, as n2)

-- Reassign using symbol literal.
n2 = Animal2.tiger
n2 = .dragon
t.eq(2, as n2)

type Animal2 enum:
    case bear
    case tiger
    case dragon

--cytest: pass