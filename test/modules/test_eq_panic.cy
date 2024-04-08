use t 'test'

t.eq(123, 234)

--cytest: error
--panic: error.AssertError
--
--main:3:1 main:
--t.eq(123, 234)
--^
--