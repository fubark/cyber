use t 'test'

-- Get runtime type.
t.eq(type(123), int)
var a = t.erase('abc')
t.eq(type(a), String)

-- id() for builtin types.
t.eq((any).id(), 10)
t.eq((bool).id(), 2)
t.eq((float).id(), 8)
t.eq((int).id(), 7)
t.eq((String).id(), 21)
t.eq((symbol).id(), 6)
t.eq((List[dyn]).id(), 13)
t.eq((Map).id(), 15)
t.eq((error).id(), 3)
t.eq((Fiber).id(), 23)
t.eq((type).id(), 11)

-- Referencing type name path.
use os
t.eq(type(os.CArray), type)

--cytest: pass