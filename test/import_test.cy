import a 'test_mods/a.cy'
import t 'test'

try t.eq(a.foo, 123)
try t.eq(valtag(a.bar), #function)
try t.eq(a.bar(), 234)