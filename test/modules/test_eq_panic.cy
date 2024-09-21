use t 'test'

t.eq(123, 234)

--cytest: error
--panic: error.AssertError
--
--test:8:9 eq:
--        throw error.AssertError
--        ^
--main:3:3 main:
--t.eq(123, 234)
--  ^
--