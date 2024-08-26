use t 'test'

-- Function with no params.
func foo():
    return 2 + 2
t.eq(foo(), 4)

-- Function with one param.
func foo1(bar int):
    return bar + 2
t.eq(foo1(1), 3)

-- Function with multiple params.
func foo2(bar int, inc int):
    return bar + inc
t.eq(foo2(20, 10), 30)

-- Static function wrapped in value.
func foo3():
    return 5
var bar = foo3
t.eq(bar(), 5)

-- Wrong number of arguments when invoking lambda.
t.eq(try bar(2), error.InvalidSignature)

-- Static function binding wrapped in value.
var bar2 = toString
t.eq(bar2(10), '10')
func toString(val int) String:
    return String(val)

-- Wrong number of arugments when invoking wrapped native func.
t.eq(try bar('a', 123), error.InvalidSignature)

-- Using as custom less function for sort.
func less(a int, b int):
    return a < b
var list = {3, 2, 1}
list.sort(less)
t.eqList(list, {1, 2, 3})

-- Single line block.
func foo5(): return 2 + 2
t.eq(foo5(), 4)

--cytest: pass