use t 'test'

var .g any = 0
var a = {_}
var b = {_}
a.append(b)
b.append(a)
g = a
var res = performGC()
t.eq(res['numCycFreed'], 0)

--cytest: pass