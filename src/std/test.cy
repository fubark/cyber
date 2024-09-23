--| Panics if `pred` is `false`.
@host fn assert(pred bool) void

--| Returns whether two values are equal.
--| Panics with `error.AssertError` if types or values do not match up.
fn eq[T](a T, b T) void:
    if !eq_(typeid[T], a, b):
        throw error.AssertError

@host -fn eq_[T](t int, a T, b T) bool

fn eqCheck[T](a T, b T) bool:
    return eq_(typeid[T], a, b)

--| TODO: Remove after `Slice` is fully implemented.
--| Returns `true` if two lists have the same size and the elements are equal
--| as if `eq` was called on those corresponding elements.
fn eqList[T](a List[T], b List[T]) void:
    if a.len() != b.len():
        fail("Length mismatch: ${a.len()} != ${b.len()}")
    for a -> a_elem, i:
        if !eqCheck(a_elem, b[i]):
            fail("Expected ${b[i]}, found ${a_elem}.\nAt element ${i}.")

--| Returns `true` if two slices have the same size and the elements are equal
--| as if `eq` was called on those corresponding elements.
fn eqSlice[T](a []T, b []T) void:
    if a.len() != b.len():
        fail("Length mismatch: ${a.len()} != ${b.len()}")
    for a -> a_elem, i:
        if !eqCheck(a_elem, b[i]):
            fail("Expected ${b[i]}, found ${a_elem}.\nAt element ${i}.")

--| Returns `true` if two numbers are near each other within epsilon 1e-5.
fn eqNear[T](a T, b T) bool:
    return eqNear_(typeid[T], a, b)

@host fn eqNear_[T](t int, a T, b T) bool

fn fail():
    throw error.AssertError

fn fail(msg string):
    eprint(msg)
    throw error.AssertError

--| Asserts that an error was thrown when invoking a function.
fn throws(func any, err error):
    var res = try func()
    if type(res) != error:
        fail('Expected error.')
    if res != err:
        fail("Expected `${err}`, found `${res}`.")
