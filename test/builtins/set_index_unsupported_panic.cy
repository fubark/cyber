1[0] = 2

--cytest: error
--panic: `func $setIndex(any, int, int) any` can not be found in `int`.
--
--main:1:1 main:
--1[0] = 2
--^
--