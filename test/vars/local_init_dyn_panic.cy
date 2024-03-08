import test

my a = test.erase(123)
var b String = a

--cytest: error
--panic: Expected type `String`, got `int` instead.
--
--main:4:16 main:
--var b String = a
--               ^
--
