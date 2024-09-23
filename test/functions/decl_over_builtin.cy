use t 'test'

var .a = 123

fn print(v int):
    a = v

print(1)
t.eq(a, 1)

--cytest: pass