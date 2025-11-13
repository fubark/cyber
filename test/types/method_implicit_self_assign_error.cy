-- Normally, this would report the use of an undeclared variable.
-- This tests that it reports a more useful error because the variable happens to be a field name of self.

type TestType:
    value int
    
fn (&TestType) set_value(new_val int):
    value = new_val

--cytest: error
--CompileError: Expected explicit `$value` or `self.value`.
--
--main:8:5:
--    value = new_val
--    ^~~~~
--