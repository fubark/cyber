if true:
    panic('Boom')

--cytest: panic
--panic: Boom
--
--[trace]
--main:2:5 main:
--    panic('Boom')
--    ^
--