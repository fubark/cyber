use a 'test_mods/a.cy'
use t 'test'

t.eq(a.g_int, 123)
t.eq(check_int(a.g_int), 123)
fn check_int(a int) -> int:
    return a

-- Access Root.g_slice.
t.eq_slice([]int{1, 2, 3}, a.g_slice)

-- Invoke function on Root.g_slice.
a.g_slice = a.g_slice + 4
t.eq_slice([]int{1, 2, 3, 4}, a.g_slice)

-- Access Root.g_arr.
t.eq_span([&]int{1, 2, 3}, a.g_arr[..])

-- Invoke function on Root.g_arr.
t.eq(3, a.g_arr.len())

t.eq(a.Vec2{x=1, y=2}, a.g_obj.downcast(^a.Vec2).*)
t.eq(3, a.g_map.size())
t.eq(1, a.g_map['a'])
t.eq(2, a.g_map['b'])
t.eq(3, a.g_map['c'])
t.eq(345, a.varFunc())
t.eq(11, a.varFunc1(10))
t.eq(30, a.varFunc2(10, 20))
type Fn1 = fn() -> int
t.eqType(Fn1, type.of(a.func))
t.eq(234, a.func())
t.eq(11, a.func1(10))
t.eq(30, a.func2(10, 20))

-- Declare a function with the same name in `a.cy`
fn sameFuncName() -> int:
    return 123
t.eq(a.sameFuncName(), sameFuncName())

-- Reference the alias `int` in `a.cy` that was already resolved in main.
t.eq(int(1), 1)
t.eq(a.useInt(1), 1)

type Vec2:
    x float
    y float
-- Same name, different object types.
v1 := Vec2{x=1, y=2}
v2 := a.Vec2{x=3, y=4}
t.eq(#{type.of(v1) != type.of(v2)}, true)
t.eq(v1.x, 1.0)
t.eq(v1.y, 2.0)
t.eq(v2.x, 3.0)
t.eq(v2.y, 4.0)

-- Calling object func from another module.
v := a.Vec2.new(3, 4)
t.eq(v.x, 3.0)
t.eq(v.y, 4.0)

-- Templates from another module.
b := a.TFoo[int]{a=123}
t.eq(b.a, 123)

--cytest: pass