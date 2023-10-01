--| Returns whether two values are equal.
--| Panics with `error.AssertError` if types or values do not match up.
@host func eq(a any, b any) any

--| Returns true if two lists have the same size and the elements are equal
--| as if `eq` was called on those corresponding elements.
@host func eqList(a any, b any) any

--| Returns two numbers are near each other within epsilon 1e-5.
@host func eqNear(a any, b any) any

@host func fail() any