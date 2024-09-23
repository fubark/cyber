use t 'test'

-- Detect end of block.
var f = fn() int:
    return 123
t.eq(f(), 123)
f = fn() int:
    var a = 123
    return a
t.eq(f(), 123)

-- Using tabs.
f = fn() int:
    return 123
t.eq(f(), 123)
f = fn() int:
    var a = 123
    return a
t.eq(f(), 123)

-- Comment before end of block.
f = fn() int:
    return 123
    -- Comment.
t.eq(f(), 123)

-- Indented comment at the end of the source.
f = fn() int:
    return 123
        -- Comment.

-- Continue from parent indentation.
f = fn() int:
    if false:
        pass
    return 123 
t.eq(f(), 123)

-- Continue from grand parent indentation.
f = fn() int:
    if false:
        if false:
            pass
    return 123 
t.eq(f(), 123)

--cytest: pass