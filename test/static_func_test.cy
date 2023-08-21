-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Function with no params.
func foo():
    return 2 + 2
t.eq(foo(), 4)

-- Function with one param.
func foo1(bar):
    return bar + 2
t.eq(foo1(1), 3)

-- Function with multiple params.
func foo2(bar, inc):
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
bar = toString
t.eq(bar(10), '10')
func toString(val) string:
    return string(val)

-- Wrong number of arugments when invoking wrapped native func.
t.eq(try bar('a', 123), error.InvalidSignature)

-- Using as custom less function for sort.
func less(a, b):
    return a < b
var list = [3, 2, 1]
list.sort(less)
t.eqList(list, [1, 2, 3])

-- Single line block.
func foo5(): return 2 + 2
t.eq(foo5(), 4)

-- Static func initializer assigns static function value.
func foo6a(val) number:
    return val
func foo6(val) number = foo6a
t.eq(foo6(123), 123)

-- Static func initializer assigns function value.
func foo7() = foo7dep
var foo7dep: func ():
    return 123
t.eq(foo7(), 123)

-- Static func initializer assigns closure value.
func foo8() = foo8dep()
var foo8dep: func ():
    var local = 123
    return func():
        return local
t.eq(foo8(), 123)