var b = 2
var foo = func (a int) int:
    return a + b
foo(1.0)

--cytest: error
--CompileError: Can not call function value.
--Expected: `func (int) int`
--Found:    `func (float)`
--
--main:4:5:
--foo(1.0)
--    ^
--