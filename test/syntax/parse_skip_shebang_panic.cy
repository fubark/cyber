#!cyber
panic('Boom')

--cytest: panic
--panic: Boom
--
--[trace]
--main:2:1 main:
--panic('Boom')
--^
--