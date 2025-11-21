use t 'test'

t.eq(123, 234)

--cytest: panic
--panic: Expected `123`, found `234`.
--
--[trace]
--main:3:1 main:
--t.eq(123, 234)
--^
--