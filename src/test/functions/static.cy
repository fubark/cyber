use t 'test'

-- Function with no params.
fn foo() -> int:
    return 2 + 2
t.eq(foo(), 4)

-- Function with one param.
fn foo1(bar int) -> int:
    return bar + 2
t.eq(foo1(1), 3)

-- Function with multiple params.
fn foo2(bar, inc int) -> int:
    return bar + inc
t.eq(foo2(20, 10), 30)

-- Static function as value.
fn foo3() -> int:
    return 5
bar := foo3
t.eq(bar(), 5)

-- Static function binding wrapped in value.
bar2 := toString
t.eq(bar2(10), '10')
fn toString(val int) -> str:
    return str(val)

-- Using as custom less function for sort.
fn less(a, b int) -> bool:
    return a < b
slice := []int{3, 2, 1}
slice.sort(less)
t.eq_slice([]int{1, 2, 3}, slice)

-- Single line block.
fn foo5() -> int: return 2 + 2
t.eq(foo5(), 4)

--cytest: pass