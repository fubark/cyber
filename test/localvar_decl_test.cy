-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Declare local and read it.
a = 1
try t.eq(a, 1)
func foo():
    -- Captured `a` from main block.
    try t.eq(a, 1)
try foo()

-- New local `a` is created in block and is different from the main block.
a = 1
func foo2():
    -- New `a` in `bar`.
    a = 3
    try t.eq(a, 3)
try foo2()
-- Main `a` remains the same.
try t.eq(a, 1)

-- Reading main `a` before creating a new local `a`
a = 1
func foo3():
    try t.eq(a, 1)
    -- New `a` in `bar`.
    a = 3
    try t.eq(a, 3)
try foo2()
try t.eq(a, 1)

-- Assign to the main `a` using capture
a = 1
func foo4():
    capture a = 3
    try t.eq(a, 3)
try foo3()
try t.eq(a, 3)

-- Subsequent assign to the main `a` using capture
a = 1
func foo5():
    capture a = 3
    a = 4
    try t.eq(a, 4)
try foo5()
try t.eq(a, 4)