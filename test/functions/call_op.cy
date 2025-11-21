-- Call op function returns a typed value.
a := 'hello' + 'world'
-- `a` should be string type.
-- TODO: Add directive to check declaration type.
b := a.concat('123')

--cytest: pass