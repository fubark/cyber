if true:
    panic('Boom')

--cytest: panic
--panic: Boom
--
--[trace]
--@MainPath():2:5 main:
--    panic('Boom')
--    ^
--