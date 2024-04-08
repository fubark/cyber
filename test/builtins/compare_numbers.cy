use t 'test'

t.eq(1 < 2, true)
t.eq(2 < 1, false)
t.eq(1 > 2, false)
t.eq(2 > 1, true)
t.eq(1 <= 2, true)
t.eq(2 <= 2, true)
t.eq(3 <= 2, false)
t.eq(1 >= 2, false)
t.eq(2 >= 2, true)
t.eq(3 >= 2, true)

--cytest: pass