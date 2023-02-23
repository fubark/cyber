-- Copyright (c) 2023 Cyber (See LICENSE)

import t 'test'

-- Declare local and read it.
a = 1
try t.eq(a, 1)
foo = func():
    -- Read captured `a` from main block.
    try t.eq(a, 1)
try foo()

-- New local `a` is created in block and is different from the main block.
a = 1
foo = func():
    -- New `a` in lambda.
    a = 3
    try t.eq(a, 3)
try foo()
-- Main `a` remains the same.
try t.eq(a, 1)

-- Assign to the main `a` using capture
a = 1
foo = func():
    capture a = 3
    try t.eq(a, 3)
try foo()
try t.eq(a, 3)

-- Subsequent assign to the main `a` using capture
a = 1
foo = func():
    capture a = 3
    a = 4
    try t.eq(a, 4)
try foo()
try t.eq(a, 4)