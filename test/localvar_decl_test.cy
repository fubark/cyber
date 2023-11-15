-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Declare local and read it.
var a = 1
t.eq(a, 1)
var foo = func():
    -- Read captured `a` from main block.
    t.eq(a, 1)
foo()

-- New local `a` is created in block and is different from the main block.
a = 1
foo = func():
    -- New `a` in lambda.
    var a = 3
    t.eq(a, 3)
foo()
-- Main `a` remains the same.
t.eq(a, 1)

-- Assign to the main `a`
a = 1
foo = func():
    a = 3
    t.eq(a, 3)
foo()
t.eq(a, 3)

-- Subsequent assign to the main `a`
a = 1
foo = func():
    a = 3
    a = 4
    t.eq(a, 4)
foo()
t.eq(a, 4)

-- Variable can be redeclared in a different sub-block or parent sub-block.
if true:
    var b = 1
    t.eq(b, 1)
if true:
    var b = 2
    t.eq(b, 2)
var b = 3
t.eq(b, 3)