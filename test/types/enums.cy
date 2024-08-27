use t 'test'

type Animal enum:
    case Bear
    case Tiger

-- enum to int.
var n = Animal.Tiger
t.eq(int(n), 1)

-- Using enum declared afterwards.
var n2 = Animal2.Tiger
t.eq(int(n2), 1)

-- Reassign using symbol literal.
n2 = Animal2.Tiger
n2 = .Dragon
t.eq(int(n2), 2)

type Animal2 enum:
    case Bear
    case Tiger
    case Dragon

--cytest: pass