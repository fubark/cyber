type S:
    a any

    func foo(self) int:
        return 123

dyn o = S{}
o.foo(234)

--cytest: error
--panic: Can not find compatible method for call: `(S) foo(int)`.
--Methods named `foo`:
--    func foo(S) int
--
--main:8:1 main:
--o.foo(234)
--^
--
