type S:
    a any

    func foo():
        return 123

var o = [S:]
o.foo(234)

--cytest: error
--CompileError: Can not find compatible function for call signature: `foo(S, int) any`.
--Functions named `foo` in `S`:
--    func foo(S) dynamic
--
--main:8:3:
--o.foo(234)
--  ^
--