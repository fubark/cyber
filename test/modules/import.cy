use a 'test_mods/a.cy'
use t 'test'

t.eq(a.varInt, 123)
t.eq(checkIntArg(a.varTypedInt), 123)
func checkIntArg(a int) int:
    return a

-- Access Root.varList.
t.eqList(a.varList, {1, 2, 3})

-- Invoke function on Root.varList.
a.varList.append(4)
t.eqList(a.varList, {1, 2, 3, 4})

t.eqList(a.varAny as List[dyn], {1, 2, 3})
t.eq(a.varMap.size(), 3)
t.eq(a.varMap['a'], 1)
t.eq(a.varMap['b'], 2)
t.eq(a.varMap['c'], 3)
t.eq(a.varFunc(), 345)
t.eq(a.varFunc1(10), 11)
t.eq(a.varFunc2(10, 20), 30)
t.eq(typeOf(a.fn), func() int)
t.eq(a.fn(), 234)
t.eq(a.fn1(10), 11)
t.eq(a.fn2(10, 20), 30)

-- Static var from another module is only initialized once.
-- This tests that ResolvedSym.genStaticInitVisited
-- prevents 2 different local syms from generating the same initializer.
t.eq(a.initOnce, 1)

-- Generates dependencies from symbol's source module and not this module.
t.eq(a.varDepRes, 123)

-- Declare a function with the same name in `a.cy`
func sameFuncName() int:
    return 123
t.eq(a.sameFuncName(), sameFuncName())

-- Reference the alias `int` in `a.cy` that was already resolved in main.
t.eq(int(1), 1)
t.eq(a.useInt(1), 1)

type Vec2:
    x float
    y float
-- Same name, different object types.
var v1 = Vec2{x=1, y=2}
var v2 = a.Vec2{x=3, y=4}
t.eq(typeOf(v1) != typeOf(v2), true)
t.eq(v1.x, 1.0)
t.eq(v1.y, 2.0)
t.eq(v2.x, 3.0)
t.eq(v2.y, 4.0)

-- Calling object func from another module.
dyn v = a.Vec2.new(3, 4)
t.eq(v.x, 3.0)
t.eq(v.y, 4.0)

-- Templates from another module.
var b a.TFoo[int] = .{a=123}
t.eq(b.a, 123)

--cytest: pass