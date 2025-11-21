use test

-- Borrow primitive.
prim := 123
prim_borrow := &prim
test.eq(123, prim_borrow.*)

-- Mutate primitive.
prim_borrow.* = 234
test.eq(234, prim_borrow.*)
test.eq(234, prim) 

-- TODO: Borrow primitive rvalue.

type Struct:
    a int

-- Borrow struct.
s := Struct{a=234}
s_borrow := &s
test.eq(234, s_borrow.a)

-- Borrow struct member.
sa_borrow := &s.a
test.eq(234, sa_borrow.*)

-- Mutate struct member.
sa_borrow.* = 345
test.eq(345, sa_borrow.*)
test.eq(345, s.a)

-- TODO: Borrow struct rvalue.

-- Borrow FixedArray.
arr := [4]int{1, 2, 3, 4}
arr_borrow := &arr
test.eq(4, arr_borrow.len())
test.eq(3, arr_borrow[2])

-- Borrow FixedArray element.
elem_borrow := &arr[2]
test.eq(3, elem_borrow.*)

-- Mutate element.
elem_borrow.* = 123
test.eq(123, elem_borrow.*)
test.eq(123, arr[2])

--cytest: pass