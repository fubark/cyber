import test

my a = test.erase(123)
var b = ''
b = a

--cytest: error
--panic: Expected type `String`, got `int` instead.
--
--main:5:5 main:
--b = a
--    ^
--