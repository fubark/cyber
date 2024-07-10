use t 'test'

t.eq(123, 234)

--cytest: error
--panic: error.AssertError
--
--test:7:12 eq:
--    return eq_((T).id(), a, b)
--           ^
--main:3:1 main:
--t.eq(123, 234)
--^
--