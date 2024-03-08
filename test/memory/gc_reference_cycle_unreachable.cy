import t 'test'

-- GC is able to detect reference cycle.
func foo():
    var a = []
    var b = []
    a.append(b)
    b.append(a)
    var res = performGC()
    -- Cycle still alive in the current stack so no gc.
    t.eq(res['numCycFreed'], 0)
foo()
var res = performGC()
t.eq(res['numCycFreed'], 2)

-- Reference cycle with child non cyclable.
func foo2():
    var a = []
    var b = []
    a.append(b)
    b.append(a)
    a.append(pointer(1))
foo2()
res = performGC()
t.eq(res['numCycFreed'], 2)

-- Reference cycle with non pool objects.
type T:
    a any
    b any
    c any
    d any
    e any
func foo3():
    var a = [T:]
    var b = [T:]
    a.c = b
    b.c = a
foo3()
res = performGC()
t.eq(res['numCycFreed'], 2)

--cytest: pass