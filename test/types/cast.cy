import t 'test'

-- Cast to exact primitive type.
my a = t.erase(1)
t.eq(foo1(a as int), true)
func foo1(a int):
    return true

-- Cast to exact object type.
a = [Foo a: 123]
t.eq(foo2(a as Foo), true)
type Foo:
    var a int
func foo2(a Foo):
    return true

-- Cast to exact string type.
a = 'abc'
t.eq(foo3(a as string), true)
func foo3(a string):
    return true

-- Cast to exact array type.
a = array('abc')
t.eq(foo4(a as array), true)
func foo4(a array):
    return true

-- Cast to abstract any type.
a = array('abc')
t.eq(foo5(a as any), true)
func foo5(a any):
    return true

-- Cast for interim dst to local.
a = t.erase(1) as int

-- Cast for interim dst to static var.
var .sa any = none
sa = t.erase(1) as int

--cytest: pass