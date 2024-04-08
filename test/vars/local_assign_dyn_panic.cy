use test

let a = test.erase(123)
var b = ''
b = a

--cytest: error
--panic: Expected type `String`, found `int`.
--
--main:5:5 main:
--b = a
--    ^
--