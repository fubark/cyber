if true:
    panic('Boom')

--cytest: error
--panic: Boom
--
--main:2:5 main:
--    panic('Boom')
--    ^
--