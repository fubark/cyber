import t 'test'

-- Cast to exact primitive type.
a = t.erase(1)
try t.eq(foo1(a as number), true)
func foo1(a number):
    return true

-- Cast to exact object type.
a = Foo{ a: 123 }
try t.eq(foo2(a as Foo), true)
object Foo:
    a number
func foo2(a Foo):
    return true

-- Cast to abstract string type.
a = 'abc'
try t.eq(foo3(a as string), true)
func foo3(a string):
    return true

-- Cast to abstract rawstring type.
a = rawstring('abc')
try t.eq(foo4(a as rawstring), true)
func foo4(a rawstring):
    return true

-- Cast to abstract any type.
a = rawstring('abc')
try t.eq(foo5(a as any), true)
func foo5(a any):
    return true