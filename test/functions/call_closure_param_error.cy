var b = 2
var foo = fn (a int) int:
    return a + b
foo(1.0)

--cytest: error
--CompileError: Can not call function value.
--Expected: `fn(int) int`
--Found:    `fn(float)`
--
--main:4:5:
--foo(1.0)
--    ^
--