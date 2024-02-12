var a ?int = none
var b = a.?

--cytest: error
--panic: Expected active choice tag `1`, found `0`.
--
--main:2:9 main:
--var b = a.?
--        ^
--