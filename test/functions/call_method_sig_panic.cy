type S:
    var a

    func foo():
        return 123

my o = [S:]
o.foo(234)

--cytest: error
--panic: Can not find compatible function for `foo(any, int) any` in `S`.
--Only `func foo(S) dynamic` exists for the symbol `foo`.
--
--main:8:1 main:
--o.foo(234)
--^
--