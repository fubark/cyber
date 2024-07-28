var a = {1, 2, 3}
a[-1]

--cytest: error
--panic: Out of bounds.
--
--main:2:1 main:
--a[-1]
--^
--