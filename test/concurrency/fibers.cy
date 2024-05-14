-- Copyright (c) 2023 Cyber (See LICENSE)

use t 'test'

-- Init fiber without starting.
func foo(list List[dyn]):
    coyield
    list.append(123)
var list = []
var f = coinit(foo, list)
t.eq(list.len(), 0)

-- Start fiber with yield at start.
func foo2(list List[dyn]):
    coyield
    list.append(123)
list = []
f = coinit(foo2, list)
coresume f
t.eq(list.len(), 0)

-- Start fiber without yield.
func foo3(list List[dyn]):
    list.append(123)
list = []
f = coinit(foo3, list)
coresume f
t.eq(list[0], 123)

-- coresume returns final value.
func foo4(list List[dyn]):
    list.append(123)
    return list[0]
list = []
f = coinit(foo4, list)
t.eq(coresume f, 123)

-- Start fiber with yield in nested function.
func bar():
    var alist = [] -- This should be released after fiber is freed.
    coyield
func foo5(list List[dyn]):
    bar()
    list.append(123)
list = []
f = coinit(foo5, list)
coresume f
t.eq(list.len(), 0)

-- Continue to resume fiber.
func foo6(list List[dyn]):
    list.append(123)
    coyield
    list.append(234)
list = []
f = coinit(foo6, list)
coresume f
coresume f
t.eq(list.len(), 2)

-- Fiber status.
func foo7():
    coyield
f = coinit(foo7)
t.eq(f.status(), .paused)
coresume f
t.eq(f.status(), .paused)
coresume f
t.eq(f.status(), .done)

-- Resuming after fiber is done is a nop.
func foo8():
    coyield
f = coinit(foo8)
coresume f
coresume f
t.eq(f.status(), .done)
coresume f
t.eq(f.status(), .done)

-- Grow fiber stack.
func sum(n int):
    if n == 0:
        return 0
    return n + sum(n - 1)
f = coinit(sum, 20)
var res = coresume f
t.eq(res, 210)

-- coinit lambda
var foof = func (list List[dyn]):
    list.append(123)
list = []
f = coinit(foof, list)
coresume f
t.eq(list[0], 123)

--cytest: pass