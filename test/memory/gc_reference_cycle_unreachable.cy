use t 'test'

-- GC is able to detect reference cycle.
fn foo():
    var a = []Object{}
    var b = []Object{}
    a.append(b as Object)
    b.append(a as Object)
    var res = collectCycles()
    -- Cycle still alive in the current stack so no gc.
    t.eq(res['num_obj_freed'], 0)
foo()
var res = collectCycles()
t.eq(res['num_obj_freed'], 2)

-- Reference cycle with child non cyclable.
fn foo2():
    var a = []Object{}
    var b = []Object{}
    a.append(b as Object)
    b.append(a as Object)
    a.append(pointer.fromAddr(void, 1))
foo2()
res = collectCycles()
t.eq(res['num_obj_freed'], 3)

-- Reference cycle with non pool objects.
type T:
    a Object
    b Object
    c Object
    d Object
    e Object
fn foo3():
    var a = ^T{}
    var b = ^T{}
    a.c = b
    b.c = a
foo3()
res = collectCycles()
t.eq(res['num_obj_freed'], 2)

--cytest: pass