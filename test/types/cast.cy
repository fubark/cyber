use t 'test'

-- Cast to exact primitive type.
dyn a = 1
t.eq(foo1(a as int), true)
fn foo1(a int) bool:
    return true

-- Cast to exact object type.
a = Foo{a=123}
t.eq(foo2(a as Foo), true)
type Foo:
    a int
fn foo2(a Foo) bool:
    return true

-- Cast to exact string type.
a = 'abc'
t.eq(foo3(a as string), true)
fn foo3(a string) bool:
    return true

-- Cast to abstract any type.
a = 'abc'
t.eq(foo5(a as any), true)
fn foo5(a any) bool:
    return true

-- Cast for interim dst to local.
dyn b = 1
a = b as int

-- Cast for interim dst to static var.
var .sa any = 'abc'
sa = b as int

--cytest: pass