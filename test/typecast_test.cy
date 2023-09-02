import t 'test'

-- Cast to exact primitive type.
var a = t.erase(1)
t.eq(foo1(a as float), true)
func foo1(a float):
    return true

-- Cast to exact object type.
a = Foo{ a: 123 }
t.eq(foo2(a as Foo), true)
type Foo object:
    a float
func foo2(a Foo):
    return true

-- Cast to abstract string type.
a = 'abc'
t.eq(foo3(a as string), true)
func foo3(a string):
    return true

-- Cast to abstract rawstring type.
a = rawstring('abc')
t.eq(foo4(a as rawstring), true)
func foo4(a rawstring):
    return true

-- Cast to abstract any type.
a = rawstring('abc')
t.eq(foo5(a as any), true)
func foo5(a any):
    return true