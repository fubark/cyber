-- Call op function returns a typed value.
var a = 'hello' + 'world'
-- `a` should be string type.
-- TODO: Add directive to check declaration type.
var b = a.concat('123')

--cytest: pass