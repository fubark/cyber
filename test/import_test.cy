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