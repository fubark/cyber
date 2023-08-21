import a 'test_mods/a.cy'
import t 'test'

t.eq(a.varNum, 123)
t.eq(checkNumberArg(a.varTypedNum), 123)
func checkNumberArg(a number):
    return a
t.eqList(a.varList, [1, 2, 3])
t.eq(a.varMap.size(), 3)
t.eq(a.varMap.a, 1)
t.eq(a.varMap.b, 2)
t.eq(a.varMap.c, 3)
t.eq(a.varFunc(), 345)
t.eq(a.varFunc1(10), 11)
t.eq(a.varFunc2(10, 20), 30)
t.eq(typesym(a.fn), #function)
t.eq(a.fn(), 234)
t.eq(a.fn1(10), 11)
t.eq(a.fn2(10, 20), 30)
t.eq(a.declAssign('123'), 123)

-- Static var from another module is only initialized once.
-- This tests that ResolvedSym.genStaticInitVisited
-- prevents 2 different local syms from generating the same initializer.
t.eq(a.initOnce, 1)

-- Generates dependencies from symbol's source module and not this module.
t.eq(a.varDepRes, 123)
t.eq(a.funcDepRes(), 123)

-- Declare a function with the same name in `a.cy`
func sameFuncName():
    return 123
t.eq(a.sameFuncName(), sameFuncName())

-- Reference the alias `number` in `a.cy` that was already resolved in main.
t.eq(number(1), 1)
t.eq(a.useNumber(1), 1)

type Vec2 object:
    x number
    y number
-- Same name, different object types.
var v1 = Vec2{ x: 1, y: 2 }
var v2 = a.Vec2{ x: 3, y: 4 }
t.eq(typeof(v1) != typeof(v2), true)
t.eq(v1.x, 1)
t.eq(v1.y, 2)
t.eq(v2.x, 3)
t.eq(v2.y, 4)

-- Calling object func from another module.
var v = a.Vec2.new(3, 4)
t.eq(v.x, 3)
t.eq(v.y, 4)