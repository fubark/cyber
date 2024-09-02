--| Panics if `pred` is `false`.
@host func assert(pred bool) void

--| Returns whether two values are equal.
--| Panics with `error.AssertError` if types or values do not match up.
func eq[T](a T, b T) bool:
    return eq_(typeid[T], a, b)

@host -func eq_[T](t int, a T, b T) bool

--| Returns `true` if two lists have the same size and the elements are equal
--| as if `eq` was called on those corresponding elements.
func eqList[T](a List[T], b List[T]) bool:
    if a.len() != b.len():
        eprint("Length mismatch: $(a.len()) != $(b.len())")
        return false
    for a -> a_elem, i:
        if !eq(a_elem, b[i]):
            return false
    return true

--| Returns `true` if two slices have the same size and the elements are equal
--| as if `eq` was called on those corresponding elements.
func eqSlice[T](a []T, b []T) bool:
    if a.len() != b.len():
        eprint("Length mismatch: $(a.len()) != $(b.len())")
        return false
    for a -> a_elem, i:
        if !eq(a_elem, b[i]):
            return false
    return true

--| Returns `true` if two numbers are near each other within epsilon 1e-5.
@host func eqNear[T](a T, b T) bool

func fail():
    throw error.AssertError

func fail(msg String):
    eprint(msg)
    throw error.AssertError

--| Asserts that an error was thrown when invoking a function.
func throws(fn any, err error):
    var res = try fn()
    if type(res) != error:
        fail('Expected error.')
    if res != err:
        fail("Expected `$(err)`, found `$(res)`.")