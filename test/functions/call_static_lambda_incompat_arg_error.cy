fn foo(a int) int:
    return a
var bar = foo
bar(1.0)

--cytest: error
--CompileError: Can not call function value.
--Expected: `fn(int) int`
--Found:    `fn(float)`
--
--main:4:5:
--bar(1.0)
--    ^
--