use test

-- TODO: Some of these tests should be moved to ref/ptr slices.

arr := [3]int{1, 2, 3}
-- test.eqArray(arr[..], [3]int{1, 2, 3}[..])

-- index operator
-- t.eq(182, arr[1-])
-- t.eq(240, arr[4-])
test.eq(1, arr[0])
test.eq(3, arr[2])

-- -- Array of structs. Tests contiguous memory layout.
-- type Foo:
--     a int
--     b int
-- slice := *[2]Foo{
--     {a=1, b=2},
--     {a=3, b=4},
-- }
-- t.eq(2, slice.len())
-- t.eq(1, slice[0].a)
-- t.eq(2, slice[0].b)
-- t.eq(3, slice[1].a)
-- t.eq(4, slice[1].b)

-- -- slice operator
-- t.eq(arr[0..], Array('abc🦊xyz🐶'))
-- t.eq(arr[7..], Array('xyz🐶'))
-- t.eq(arr[10..], Array('🐶'))
-- t.eq(try arr[-1..], error.OutOfBounds)
-- t.eq(try arr[..-1], error.OutOfBounds)
-- t.eq(try arr[15..], error.OutOfBounds)
-- t.eq(try arr[..15], error.OutOfBounds)
-- t.eq(try arr[14..15], error.OutOfBounds)
-- t.eq(try arr[3..1], error.OutOfBounds)
-- t.eq(arr[14..], Array(''))
-- t.eq(arr[..0], Array(''))
-- t.eq(arr[..7], Array('abc🦊'))
-- t.eq(arr[..10], Array('abc🦊xyz'))
-- t.eq(arr[..14], Array('abc🦊xyz🐶'))
-- t.eq(arr[0..0], Array(''))
-- t.eq(arr[0..1], Array('a'))
-- t.eq(arr[7..14], Array('xyz🐶'))
-- t.eq(arr[10..14], Array('🐶'))
-- t.eq(arr[14..14], Array(''))

-- -- concat()
-- t.eq(arr.concat(Array('123')), Array('abc🦊xyz🐶123'))

-- -- endsWith()
-- t.eq(arr.endsWith(Array('xyz🐶')), true)
-- t.eq(arr.endsWith(Array('xyz')), false)

-- -- find()
-- t.eq(arr.find(Array('bc🦊')).?, 1)
-- t.eq(arr.find(Array('xy')).?, 7)
-- t.assert(arr.find(Array('bd')) == none)
-- t.eq(arr.find(Array('ab')).?, 0)

-- -- findAnyByte()
-- t.eq(arr.findAnyByte(Array('a')).?, 0)
-- t.eq(arr.findAnyByte(Array('xy')).?, 7)
-- t.assert(arr.findAnyByte(Array('ef')) == none)

-- -- findByte()
-- t.eq(arr.findByte(`a`).?, 0)
-- t.eq(arr.findByte(`x`).?, 7)
-- t.assert(arr.findByte(`d`) == none)
-- t.eq(arr.findByte(97).?, 0)
-- t.assert(arr.findByte(100) == none)

-- -- insert()
-- t.eq(try arr.insert(-1, Array('foo')), error.OutOfBounds)
-- t.eq(arr.insert(0, Array('foo')), Array('fooabc🦊xyz🐶'))
-- t.eq(arr.insert(3, Array('foo🦊')), Array('abcfoo🦊🦊xyz🐶'))
-- t.eq(arr.insert(10, Array('foo')), Array('abc🦊xyzfoo🐶'))
-- t.eq(arr.insert(14, Array('foo')), Array('abc🦊xyz🐶foo'))
-- t.eq(try arr.insert(15, Array('foo')), error.OutOfBounds)

-- len()
test.eq(3, arr.len())

-- -- repeat()
-- t.eq(try arr.repeat(-1), error.InvalidArgument)
-- t.eq(arr.repeat(0), Array(''))
-- t.eq(arr.repeat(1), Array('abc🦊xyz🐶'))
-- t.eq(arr.repeat(2), Array('abc🦊xyz🐶abc🦊xyz🐶'))

-- -- replace()
-- t.eq(arr.replace(Array('abc🦊'), Array('foo')), Array('fooxyz🐶'))
-- t.eq(arr.replace(Array('bc🦊'), Array('foo')), Array('afooxyz🐶'))
-- t.eq(arr.replace(Array('bc'), Array('foo🦊')), Array('afoo🦊🦊xyz🐶'))
-- t.eq(arr.replace(Array('xy'), Array('foo')), Array('abc🦊fooz🐶'))
-- t.eq(arr.replace(Array('xyz🐶'), Array('foo')), Array('abc🦊foo'))
-- t.eq(arr.replace(Array('abcd'), Array('foo')), Array('abc🦊xyz🐶'))

-- -- split()
-- res := Array('abc,🐶ab,a').split(Array(','))
-- t.eq(res.len(), 3)
-- t.eq(res[0], Array('abc'))
-- t.eq(res[1], Array('🐶ab'))
-- t.eq(res[2], Array('a'))

-- -- trim()
-- t.eq(arr.trim(.left, Array('a')), Array('bc🦊xyz🐶'))
-- t.eq(arr.trim(.right, Array('🐶')), Array('abc🦊xyz'))
-- t.eq(arr.trim(.ends, Array('a🐶')), Array('bc🦊xyz'))

-- -- startsWith()
-- t.eq(arr.startsWith(Array('abc🦊')), true)
-- t.eq(arr.startsWith(Array('bc🦊')), false)

--cytest: pass