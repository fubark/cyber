-- This file should produce a compile error
-- Test: Ambiguous field assignment without explicit self.

type TestType:
    value int
    
    func trySetValue(self, new_val int):
        value = new_val  -- ERROR: Cannot assign to field without explicit 'self.'
