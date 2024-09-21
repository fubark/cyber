var foo = isDigit
foo(1.0)

--cytest: error
--CompileError: Can not call function value.
--Expected: `func (int) bool`
--Found:    `func (float)`
--
--main:2:5:
--foo(1.0)
--    ^
--