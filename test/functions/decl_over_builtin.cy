use t 'test'

var .a = 123

func print(v int):
    a = v

print(1)
t.eq(a, 1)

--cytest: pass