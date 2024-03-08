import test

-- Explicit type.
var a Option(int) = 123
-- test.eq(a == none, false)
-- test.eq(a != none, true)
test.eq(a.?, 123)

-- Shorthand.
var b ?int = 123
test.eq(b.?, 123)

-- Wrap object.
type Foo:
    a int
var c ?Foo = [Foo a: 123]
test.eq(c.?.a, 123)

-- Wrapping none implicitly.
b = none
-- test.eq(b == none, true)
-- test.eq(b != none, false)

-- -- Unwrap or default.
-- b = none
-- test.eq(b ?else 123, 123)

--cytest: pass