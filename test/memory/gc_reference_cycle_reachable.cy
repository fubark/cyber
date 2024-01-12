import t 'test'

var Root.g = none
var a = []
var b = []
a.append(b)
b.append(a)
g = a
var res = performGC()
t.eq(res['numCycFreed'], 0)

--cytest: pass