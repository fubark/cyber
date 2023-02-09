import a 'test_mods/a.cy'
import t 'test'

try t.eq(a.varNum, 123)
try t.eqList(a.varList, [1, 2, 3])
try t.eq(a.varMap.size(), 3)
try t.eq(a.varMap.a, 1)
try t.eq(a.varMap.b, 2)
try t.eq(a.varMap.c, 3)
try t.eq(a.varFunc(), 345)
try t.eq(a.varFunc1(10), 11)
try t.eq(a.varFunc2(10, 20), 30)
try t.eq(valtag(a.fn), #function)
try t.eq(a.fn(), 234)
try t.eq(a.fn1(10), 11)
try t.eq(a.fn2(10, 20), 30)
try t.eq(a.declAssign('123'), 123)

-- Static var from another module is only initialized once.
-- This tests that ResolvedSym.genStaticInitVisited
-- prevents 2 different local syms from generating the same initializer.
try t.eq(a.initOnce, 1)

-- Generates dependencies from symbol's source module and not this module.
try t.eq(a.varDepRes, 123)
try t.eq(a.funcDepRes(), 123)