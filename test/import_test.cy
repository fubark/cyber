import a 'test_mods/a.cy'
import t 'test'

try t.eq(a.varNum, 123)
try t.eqList(a.varList, [1, 2, 3])
try t.eq(a.varMap.size(), 3)
try t.eq(a.varMap.a, 1)
try t.eq(a.varMap.b, 2)
try t.eq(a.varMap.c, 3)
try t.eq(valtag(a.bar), #function)
try t.eq(a.bar(), 234)