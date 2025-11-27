#!cyber
panic('Boom')

--cytest: panic
--panic: Boom
--
--[trace]
--@MainPath():2:1 main:
--panic('Boom')
--^
--