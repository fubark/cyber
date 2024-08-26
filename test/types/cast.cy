use t 'test'

-- Cast to exact primitive type.
let a = t.erase(1)
t.eq(foo1(a as int), true)
func foo1(a int) bool:
    return true

-- Cast to exact object type.
a = Foo{a=123}
t.eq(foo2(a as Foo), true)
type Foo:
    a int
func foo2(a Foo) bool:
    return true

-- Cast to exact string type.
a = 'abc'
t.eq(foo3(a as String), true)
func foo3(a String) bool:
    return true

-- Cast to abstract any type.
a = 'abc'
t.eq(foo5(a as any), true)
func foo5(a any) bool:
    return true

-- Cast for interim dst to local.
a = t.erase(1) as int

-- Cast for interim dst to static var.
var .sa any = 'abc'
sa = t.erase(1) as int

--cytest: pass