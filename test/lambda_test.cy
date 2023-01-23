-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- No params.
foo = () => 2 + 2
try t.eq(foo(), 4)

-- One param.
foo = a => a + 1
try t.eq(foo(10), 11)

-- Multiple params.
foo = (bar, inc) => bar + inc
try t.eq(foo(20, 10), 30)

-- Pass lambda as arg.
func call(f):
    return f(14)
try t.eq(call(a => a + 1), 15)

-- Using parentheses.
m = { a: () => 4 }
try t.eq((m.a)(), 4)

-- -- Invoking lambda temp.
-- try t.eq((a => a + 1)(14), 15)

-- Multi-line lambda, no params.
foo = func ():
    return 2 + 2
try t.eq(foo(), 4)

-- Multi-line lambda, one param.
foo = func (a):
    return a + 1
try t.eq(foo(10), 11)

-- Multi-line lambda, multiple params.
foo = func (bar, inc):
    return bar + inc
try t.eq(foo(20, 10), 30)

-- Multi-line lambda, single line block.
foo = func (): return 2 + 2
try t.eq(foo(), 4)