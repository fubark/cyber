use t 'test'

-- Test: Correct field assignment with explicit self.
type CorrectType:
    value int
    
    func setValue(self, new_val int):
        self.value = new_val  -- Correct: explicit self.
    
    func getValue(self) int:
        return value  -- OK: reading field is allowed

var correct = CorrectType{value=42}
t.eq(correct.getValue(), 42)

-- Test: Local variable with same name as field is allowed
type LocalVarType:
    value int
    
    func useLocalVar(self) int:
        var value = 100  -- OK: declares new local variable
        return value  -- Returns local, not field

var local_test = LocalVarType{value=42}
t.eq(local_test.useLocalVar(), 100)
t.eq(local_test.value, 42)  -- Field unchanged

--cytest: pass
