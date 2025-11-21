a := 123
b := fn():
    return a

--cytest: error
--CompileError: Can only capture variables declared with the reference types `^T` or `&T`.
--
--main:3:12:
--    return a
--           ^
--