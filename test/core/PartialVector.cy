use test

-- Initialize empty.
arr := [..3]int{}
test.eq(0, arr.len())

-- Initialize partial.
arr = [..3]int{1}
test.eq(1, arr.len())
test.eq(1, arr[0])

-- Update index.
arr[0] = 2
test.eq(1, arr.len())
test.eq(2, arr[0])

-- Append.
arr.append(3)
test.eq(2, arr.len())
test.eq(3, arr[1])

-- Initialize full.
arr = [..3]int{1, 2, 3}
test.eq(3, arr.len())
test.eq(1, arr[0])
test.eq(2, arr[1])
test.eq(3, arr[2])

--cytest: pass