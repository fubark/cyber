use test

c := 123
ret := ret_borrow_param(&c)
test.eq(123, ret.*)

s := S{inner=234}
ret2 := ret_borrow_access(&s)
test.eq(234, ret2.*)

arr := [2]int{0, 345}
ret3 := ret_borrow_index(&arr)
test.eq(345, ret3.*)

type S:
    inner int

fn ret_borrow_param(scope a &int) -> scope &int:
    return a

fn ret_borrow_access(scope a &S) -> scope &int:
    return &a.inner

fn ret_borrow_index(scope a &[2]int) -> scope &int:
    return &a[1]

--cytest: pass