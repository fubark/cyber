import t 'test'

func print(v):
    return v + 2

t.eq(print(1), 3)

--cytest: pass