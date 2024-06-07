use t 'test'

-- Op assign to variable.
let a = 1
a += 10
t.eq(a, 11)

a = 100
a *= 2
t.eq(a, 200)
a /= 4
t.eq(a, 50)
a -= 1
t.eq(a, 49)

-- Op assign to field.
type S:
    foo int
var s = S{foo=1}
s.foo += 10
t.eq(s.foo, 11)

-- Op assign to index.
a = [1]
a[0] += 1
t.eq(a[0], 2)

--cytest: pass