use t 'test'

-- Basic.
var list = [1, 2, 3]
var sum = 0
for list -> it:
    sum += it
t.eq(sum, 6)

-- From static iterable.
var .sList = [1, 2, 3]
sum = 0
for sList -> it:
    sum += it
t.eq(sum, 6)

-- Loop item var shadows parent var.
var elem = 123
sum = 0
list = [1, 2, 3]
for list -> elem:
    sum += elem
t.eq(sum, 6)
t.eq(elem, 123)

-- Break.
list = [1, 2, 3]
sum = 0
for list -> it:
    if it == 3:
        break
    sum += it
t.eq(sum, 3)

-- Continue.
list = [1, 2, 3]
sum = 0
for list -> it:
    if it == 1:
        continue
    sum += it
t.eq(sum, 5)

-- Single line block.
list = [1, 2, 3]
sum = 0
for list -> it: sum += it
t.eq(sum, 6)

-- Return expr inside loop body.
list = [1, 2, 3]
var f = func (arr List[dynamic]):
    for arr -> item:
        if item == 4:
            return 1
        else item == 5:
            return 1
    return 0
t.eq(f(list), 0)

-- Empty iterator. Tests that iterator is cleaned up without entering body loop.
list = []
var count = 0
for list -> it:
    count += 1
t.eq(count, 0)

--cytest: pass