use t 'test'

var list = []
for 0..10 -> i:
   list.append(i)
t.eq(list[9], 9)

--cytest: pass