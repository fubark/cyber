use t 'test'

-- Omits last comma for multiline initializer.
a := []int{
    1,
    2,
}
t.eq(a.len(), 2)

-- Construct slice.
ta := []int{1, 2, 3}
t.eqType([]int, type.of(ta))
t.eq(ta.len(), 3)

-- To target type.
ta = {2, 3, 4}

-- Index access.
ta = {1, 2, 3}
t.eq(ta[0], 1)
t.eq(ta[1], 2)
t.eq(ta[2], 3)

--| Slice operator.
-- Start to end index slice.
a = {1, 2, 3, 4, 5}
t.eq_slice(a[1..4], {2, 3, 4})
-- Start index to end of slice.
t.eq_slice(a[3..], {4, 5})
-- Start of slice to end index.
t.eq_slice(a[0..3], {1, 2, 3})

--| Slice of managed elements.
a2 := []^int{^1, ^2, ^3}
a2_view := a2[0..1]
t.eq(1, a2_view[0].*)

-- Set index
a = {}
a = a.size_up(3, 0)
a[2] = 3
t.eq(a[2], 3)

-- <<, append()
a = {}
a = a << 1
t.eq(a.len(), 1)
t.eq(a[0], 1)

-- <<, append([]T)
a = {1, 2, 3}
a = a << {4, 5, 6}
t.eq_slice([]int{1, 2, 3, 4, 5, 6}, a)

-- insert() in empty
a = {}
a = a.insert(0, 1)
t.eq(a[0], 1)

-- insert() at start
a = a.insert(0, 2)
t.eq(2, a.len())
t.eq_slice([]int{2, 1}, a)

-- insert() at end
a = a.insert(2, 3)
t.eq_slice([]int{2, 1, 3}, a)

-- insert() in middle
a = a.insert(1, 4)
t.eq_slice([]int{2, 4, 1, 3}, a)

-- TODO: insert() at index out of bounds.

-- -- join()
-- t.eq([]int{}.join(','), '')
-- t.eq({1}.join(','), '1')
-- t.eq({1, 2, 3}.join(','), '1,2,3')
-- t.eq({1, 2, 3}.join(',').isAscii(), true)
-- t.eq({1, 2, 3}.join('🦊'), '1🦊2🦊3')
-- t.eq({1, 2, 3}.join('🦊').isAscii(), false)

-- len()
a = {1, 2, 3, 4}
t.eq(4, a.len())

-- remove()
a = {1, 2, 3}
a = a.remove(1)
t.eq(2, a.len())
t.eq(1, a[0])
t.eq(3, a[1])

-- remove() first item.
a = {1, 2, 3}
a = a.remove(0)
t.eq(2, a.len())
t.eq(2, a[0])
t.eq(3, a[1])

-- remove() last item.
a = {1, 2, 3}
a = a.remove(2)
t.eq(2, a.len())
t.eq(1, a[0])
t.eq(2, a[1])

-- TODO: remove() out of bounds.

-- remove() rc item.
a2 = {^1, ^2, ^3}
a2 = a2.remove(1)
t.eq(2, a2.len())
t.eq(1, a2[0].*)
t.eq(3, a2[1].*)

-- size_up/size_down()
a = {1, 2, 3}
a = a.size_up(4, 0)
t.eq(4, a.len())
t.eq(0, a[3])
a = a.size_down(2)
t.eq(2, a.len())
t.eq(2, a[1])

-- sort()
a = {3, 1, 2}
a.sort(|a, b| a < b)
t.eq_slice([]int{1, 2, 3}, a)
a2 = { ^3, ^1, ^2 }
a2.sort(|a, b| a.* < b.*)
t.eq(1, a2[0].*)
t.eq(2, a2[1].*)
t.eq(3, a2[2].*)

-- Iteration.
a = {1, 2, 3, 4, 5}
sum := 0
for a |it|:
    sum += it
t.eq(15, sum)

-- Pair iteration.
a = {10, 20, 30}
sum = 0
idxSum := 0
for a |idx, it|:
    sum += it
    idxSum += idx
t.eq(60, sum)
t.eq(3, idxSum)

-- Nested iteration.
a = {1, 2, 3}
res := 0
for a |n|:
    innerSum := 0
    for a |m|:
        innerSum += m
    res += n * innerSum
t.eq(36, res)

-- Nested pair iteration.
a = {1, 2, 3}
res = 0
idxRes := 0
for a |i, n|:
    innerSum := 0
    idxSum = 0
    for a |j, m|:
        innerSum += m
        idxSum += j
    res += n * innerSum
    idxRes += i * idxSum 
t.eq(36, res)
t.eq(9, idxRes)

-- `@init` with primitive.
a = []int(10, 123)
t.eq(10, a.len())
for 0..10 |i|:
    t.eq(123, a[i])

-- `@init` with ref.
a3 := []^int(2, ^1)
t.eq(2, a3.len())
t.eq(true, a3[0] == a3[1])

-- Growing array destructs managed elements.
grow_arr := []str{}
for 0..100 |i|:
    grow_arr = grow_arr << str(i)

--cytest: pass