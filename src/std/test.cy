--| Panics if `pred` is `false`.
@host func assert(pred bool) void

--| Returns whether two values are equal.
--| Panics with `error.AssertError` if types or values do not match up.
@host func eq(a any, b any) bool

--| Returns `true` if two lists have the same size and the elements are equal
--| as if `eq` was called on those corresponding elements.
@host func eqList(a any, b any) bool

--| Returns `true` if two numbers are near each other within epsilon 1e-5.
@host func eqNear(a any, b any) bool

func fail():
    throw error.AssertError

func fail(msg String):
    eprint(msg)
    throw error.AssertError

--| Asserts that an error was thrown when invoking a function.
func throws(fn any, err error):
    var res = try fn()
    if typeof(res) != error:
        fail('Expected error.')
    if res != err:
        fail("Expected `$(err)`, found `$(res)`.")