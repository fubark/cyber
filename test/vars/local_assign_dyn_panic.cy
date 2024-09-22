use test

dyn a = 123
var b = ''
b = a

--cytest: error
--panic: Expected type `string`, found `int`.
--
--main:5:5 main:
--b = a
--    ^
--