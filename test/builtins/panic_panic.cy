var a = 123
1 + panic(.boom)

--cytest: error
--panic: .boom
--
--main:2:5 main:
--1 + panic(.boom)
--    ^
--