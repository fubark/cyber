var foo = isDigit
foo(1.0)

--cytest: error
--CompileError: Can not call function value.
--Expected: `fn(int) bool`
--Found:    `fn(float)`
--
--main:2:5:
--foo(1.0)
--    ^
--