use test

a := 123
ptr := *a
span := ptr[0..1]

-- @index_addr
test.eq(span[0], 123)

-- len()
test.eq(span.len(), 1)

-- @index_addr set.
span[0] = 234
test.eq(span[0], 234)

-- Iterator.
arr := [_]int{1, 2, 3}
span2 := arr.as_ptr_span()
sum := 0
for span2 |n|:
    sum += n
test.eq(6, sum)

--cytest: pass