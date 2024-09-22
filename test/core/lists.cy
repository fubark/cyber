use t 'test'

-- Omits last comma for multiline initializer.
var a = {
    1,
    2,
}
t.eq(a.len(), 2)

-- Explicit list type.
var ta = List[int]{1, 2, 3}
t.eq(typeOf(ta), List[int])
t.eq(ta.len(), 3)
t.eq(ta[0], 1)

-- Infer list type.
var int_list List[int] = .{1, 2, 3}
t.eq(int_list.len(), 3)
t.eq(int_list[0], 1)
t.eq(int_list[1], 2)
t.eq(int_list[2], 3)

-- Index access.
a = .{1, 2, 3}
t.eq(a[0], 1)

--| Slice operator.
-- Start to end index slice.
a = .{1, 2, 3, 4, 5}
t.eqList(a[1..4], .{2, 3, 4})
-- Start index to end of list.
t.eqList(a[3..], .{4, 5})
-- Start of list to end index.
t.eqList(a[..3], .{1, 2, 3})

--| Slice retains rc elems.
var a2 = List[dyn]{string(1), string(2), string(3)}
a2 = a2[..1]
t.eq(a2[0], '1')

-- Set index
a = .{}
a.resize(3)
a[2] = 3
t.eq(a[2], 3)

-- append()
a = .{}
a.append(1)
t.eq(a.len(), 1)
t.eq(a[0], 1)

-- appendAll(List)
a = .{1, 2, 3}
a.appendAll(.{4, 5, 6})
t.eqList(a, .{1, 2, 3, 4, 5, 6})

-- insert() in empty
a = .{}
a.insert(0, 1)
t.eq(a[0], 1)

-- insert() at start
a.insert(0, 2)
t.eqList(a, .{2, 1})

-- insert() at end
a.insert(2, 3)
t.eqList(a, .{2, 1, 3})

-- insert() in middle
a.insert(1, 4)
t.eqList(a, .{2, 4, 1, 3})

-- insert() at index out of bounds.
try:
    a.insert(-1, 123)
catch err:
    t.eq(err, error.OutOfBounds)
try:
    a.insert(100, 123)
catch err:
    t.eq(err, error.OutOfBounds)

-- join()
t.eq(List[int]{}.join(','), '')
t.eq({1}.join(','), '1')
t.eq({1, 2, 3}.join(','), '1,2,3')
t.eq({1, 2, 3}.join(',').isAscii(), true)
t.eq({1, 2, 3}.join('🦊'), '1🦊2🦊3')
t.eq({1, 2, 3}.join('🦊').isAscii(), false)

-- len()
a = .{1, 2, 3, 4}
t.eq(a.len(), 4)

-- remove()
a = .{1, 2, 3}
a.remove(1)
t.eq(a.len(), 2)
t.eq(a[0], 1)
t.eq(a[1], 3)

-- remove() first item.
a = .{1, 2, 3}
a.remove(0)
t.eq(a.len(), 2)
t.eq(a[0], 2)
t.eq(a[1], 3)

-- remove() last item.
a = .{1, 2, 3}
a.remove(2)
t.eq(a.len(), 2)
t.eq(a[0], 1)
t.eq(a[1], 2)

-- remove() out of bounds.
a = .{1, 2, 3}
try:
    a.remove(-1)
catch err:
    t.eq(err, error.OutOfBounds)
try:
    a.remove(3)
catch err:
    t.eq(err, error.OutOfBounds)
t.eq(a.len(), 3)

-- remove() rc item.
a2 = .{1, 'abc', 3}
a2.remove(1)
t.eq(a2.len(), 2)
t.eq(a2[0], 1)
t.eq(a2[1], 3)

-- resize()
a = .{1, 2, 3}
a.resize(4)
t.eq(a.len(), 4)
t.eq(a[3], 0)
a.resize(2)
t.eq(a.len(), 2)
t.eq(a[1], 2)

-- sort()
a = .{3, 1, 2}
a.sort((a, b) => a < b)
t.eqList(a, .{1, 2, 3})
a2 = .{ {3}, {1}, {2} }
a2.sort((a, b) => a[0] < b[0])
t.eq(a2[0][0], 1)
t.eq(a2[1][0], 2)
t.eq(a2[2][0], 3)

-- Iteration.
a = .{1, 2, 3, 4, 5}
var sum = 0
for a -> it:
    sum += it
t.eq(sum, 15)

-- Pair iteration.
a = .{10, 20, 30}
sum = 0
var idxSum = 0
for a -> it, idx:
    sum += it
    idxSum += idx
t.eq(sum, 60)
t.eq(idxSum, 3)

-- Nested iteration.
a = .{1, 2, 3}
var res = 0
for a -> n:
    var innerSum = 0
    for a -> m:
        innerSum += m
    res += n * innerSum
t.eq(res, 36)

-- Nested pair iteration.
a = .{1, 2, 3}
res = 0
var idxRes = 0
for a -> n, i:
    var innerSum = 0
    idxSum = 0
    for a -> m, j:
        innerSum += m
        idxSum += j
    res += n * innerSum
    idxRes += i * idxSum 
t.eq(res, 36)
t.eq(idxRes, 9)

-- List.fill with primitive.
func testFill():
    var a = List.fill(123, 10)
    t.eq(a.len(), 10)
    for 0..10 -> i:
        t.eq(a[i], 123)
testFill()

-- List.fill with object performs shallow copy.
func testFill2():
    var a = List.fill({}, 2)
    t.eq(a.len(), 2)
    t.eq(a[0] == a[1], false)
testFill2()

--cytest: pass