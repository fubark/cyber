use t 'test'

func print(v int):
    return v + 2

t.eq(print(1), 3)

--cytest: pass