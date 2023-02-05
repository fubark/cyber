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

-- Static function closure wrapped in value.
a = 123
func foo4():
    return a
bar = foo4
try t.eq(bar(), 123)

-- Wrong number of arugments when invoking closure.
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

-- Assign to function declaration with static function value.
func foo6(val) = number
try t.eq(foo6('123'), 123)