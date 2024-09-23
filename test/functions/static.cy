use t 'test'

-- Function with no params.
fn foo() int:
    return 2 + 2
t.eq(foo(), 4)

-- Function with one param.
fn foo1(bar int) int:
    return bar + 2
t.eq(foo1(1), 3)

-- Function with multiple params.
fn foo2(bar int, inc int) int:
    return bar + inc
t.eq(foo2(20, 10), 30)

-- Static function wrapped in value.
fn foo3() int:
    return 5
dyn bar = foo3
t.eq(bar(), 5)

-- Wrong number of arguments when invoking lambda.
t.eq(try bar(2), error.InvalidSignature)

-- Static function binding wrapped in value.
var bar2 = toString
t.eq(bar2(10), '10')
fn toString(val int) string:
    return string(val)

-- Wrong number of arugments when invoking wrapped native func.
t.eq(try bar('a', 123), error.InvalidSignature)

-- Using as custom less function for sort.
fn less(a int, b int) bool:
    return a < b
var list = {3, 2, 1}
list.sort(less)
t.eqList(list, {1, 2, 3})

-- Single line block.
fn foo5() int: return 2 + 2
t.eq(foo5(), 4)

--cytest: pass