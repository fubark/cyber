use t 'test'

var .g any = 0
var a = List[dyn]{}
var b = List[dyn]{}
a.append(b)
b.append(a)
g = a
var res = performGC()
t.eq(res['numCycFreed'], 0)

--cytest: pass