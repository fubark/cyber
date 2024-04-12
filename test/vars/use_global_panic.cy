use test
use $global

b = a

--cytest: error
--panic: Variable is not defined in `$global`.
--
--main:4:5 main:
--b = a
--    ^
--