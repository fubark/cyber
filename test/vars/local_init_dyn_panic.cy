use test

dyn a = test.erase(123)
var b String = a

--cytest: error
--panic: Expected type `String`, found `int`.
--
--main:4:16 main:
--var b String = a
--               ^
--
