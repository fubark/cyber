use t 'test'

var .g any = 0
var a = List[dyn]{}
var b = List[dyn]{}
a.append(b)
b.append(a)
g = a
var res = collectCycles()
t.eq(res['num_obj_freed'], 0)

--cytest: pass