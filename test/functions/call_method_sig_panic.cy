use test

type S:
    a any

    func foo():
        return 123

let o = test.erase(S{})
o.foo(234)

--cytest: error
--panic: Can not find compatible method for call: `(S) foo(int)`.
--Methods named `foo`:
--    func foo(S) dynamic
--
--main:10:1 main:
--o.foo(234)
--^
--