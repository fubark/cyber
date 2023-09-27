import t 'test'

-- id()
t.eq(typeof(none).id(), 0)
t.eq(typeof(true).id(), 1)
t.eq(typeof(false).id(), 1)
t.eq(typeof(error.err).id(), 2)
t.eq(typeof('abc').id(), 3)
t.eq(typeof('abc🦊').id(), 4)
t.eq(typeof(.abc).id(), 6)
t.eq(typeof(123).id(), 7)
t.eq(typeof(123.0).id(), 8)
t.eq(typeof([]).id(), 10)
t.eq(typeof({}).id(), 12)

-- Referencing type object.
type Foo object:
	a float
var foo = Foo{ a: 123 }
t.eq(typeof(foo), Foo)

-- Referencing builtin types.
t.eq((any).id(), 27)
t.eq((boolean).id(), 1)
t.eq((float).id(), 8)
t.eq((int).id(), 7)
t.eq((string).id(), 28)
t.eq((rawstring).id(), 29)
t.eq((symbol).id(), 6)
t.eq((List).id(), 10)
t.eq((Map).id(), 12)
t.eq((pointer).id(), 25)
t.eq((error).id(), 2)
t.eq((fiber).id(), 21)
t.eq((metatype).id(), 26)

-- Referencing type name path.
import os 'os'
t.eq(typesym(os.CFunc), .metatype)