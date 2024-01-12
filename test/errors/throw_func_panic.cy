func foo():
    throw error.boom

foo()

--cytest: error
--panic: error.boom
--
--main:2:5 foo:
--    throw error.boom
--    ^
--main:4:1 main:
--foo()
--^
--