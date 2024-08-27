use t 'test'

func fail() int:
    throw error.Fail

func happy(a int) int:
    return a

-- No error.
dyn a = false
try:
    a = 1
catch:
    a = 0
t.eq(a, 1)

-- No error in function.
func foo() int:
    try:
        return 1
    catch:
        return 0
t.eq(foo(), 1)

-- Error case.
try:
    a = fail()
catch:
    a = 0
t.eq(a, 0)

-- Error case, capture error.
try:
    a = fail()
catch err:
    a = err
t.eq(a, error.Fail)

-- Error case in function
func foo2() int:
    try:
        return fail()
    catch:
        return 0
t.eq(foo2(), 0)

-- Error case in sub expr.
try:
    a = happy(1)
    a = happy(fail())
    a = 1
catch:
    a = 0
t.eq(a, 0)

-- Throw from nested call.
func foo4():
    fail()   
func foo3():
    foo4()
try:
    foo3()
    a = 1
catch:
    a = 0
t.eq(a, 0)

-- Catch invokes lambda release inst.
-- If it's skipped, this test would report an unreleased ref count.
var f = func():
    throw error.Fail
try:
    f()
    t.fail()
catch:
    pass

-- Catch invokes lambda release inst one frame down.
func foo5(f dyn):
    f()
try:
    foo5(f)
    t.fail()
catch:
    pass

--cytest: pass