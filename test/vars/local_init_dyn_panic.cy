use test

dyn a = 123
var b string = a

--cytest: error
--panic: Expected type `string`, found `int`.
--
--main:4:16 main:
--var b string = a
--               ^
--
