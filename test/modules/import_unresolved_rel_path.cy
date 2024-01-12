import a './test_mods/../test_mods/a.cy'
import t 'test'
t.eq(a.varInt, 123)

--cytest: pass