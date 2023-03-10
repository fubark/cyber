-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Function with no params.
func foo():
    return 2 + 2
try t.eq(foo(), 4)

-- Function with one param.
func foo1(bar):
    return bar + 2
try t.eq(foo1(1), 3)

-- Function with multiple params.
func foo2(bar, inc):
    return bar + inc
try t.eq(foo2(20, 10), 30)

-- Static function wrapped in value.
func foo3():
    return 5
bar = foo3
try t.eq(bar(), 5)

-- Wrong number of arguments when invoking lambda.
try t.eq(bar(2), error(#InvalidSignature))

-- Static function binding wrapped in value.
bar = asciiCode
try t.eq(bar('a'), 97)

-- Wrong number of arugments when invoking wrapped native func.
try t.eq(bar('a', 123), error(#InvalidSignature))

-- Using as custom less function for sort.
func less(a, b):
    return a < b
list = [3, 2, 1]
list.sort(less)
try t.eqList(list, [1, 2, 3])

-- Single line block.
func foo5(): return 2 + 2
try t.eq(foo5(), 4)

-- Static func initializer assigns static function value.
func foo6(val) = number
try t.eq(foo6('123'), 123)

-- Static func initializer assigns function value.
func foo7() = foo7dep
var foo7dep: func ():
    return 123
try t.eq(foo7(), 123)

-- Static func initializer assigns closure value.
func foo8() = foo8dep()
var foo8dep: func ():
    local = 123
    return func():
        return local
try t.eq(foo8(), 123)